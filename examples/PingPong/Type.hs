{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE QualifiedDo #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}
{-# LANGUAGE NumericUnderscores #-}

module Type where

import Control.Monad.Class.MonadSay
import Data.Binary
import Data.Binary.Get
import Data.Binary.Put
import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder.Extra as L
import qualified Data.ByteString.Lazy as L
import Data.IFunctor (At (..), Sing, SingI, ireturn, returnAt)
import qualified Data.IFunctor as I
import Data.Kind
import GHC.Exts (dataToTag#)
import GHC.Int (Int (I#))
import qualified Network.Socket as Socket
import qualified Network.Socket.ByteString as Socket
import TypedProtocol.Codec
import TypedProtocol.Core
import TypedProtocol.Driver

{-

--------------------------------------------------------------------------
   Client                               Server
    :S0 s                                 :S0 s
  -----------------------------------------------------------------------
  | :S0 True                           :S0 s
  |             Ping    ->
  | :S1                                :S1
  |             <- Pong
  | :S0 s                               :S0 s
  -----------------------------------------------------------------------

  -----------------------------------------------------------------------
  |  :S0 False                          :S0 s
  |               Stop    ->
  |  :End                               :End
  -----------------------------------------------------------------------
 -}

data Role = Client | Server
  deriving (Show, Eq, Ord)

data SRole :: Role -> Type where
  SClient :: SRole Client
  SServer :: SRole Server

type instance Sing = SRole

instance SingI Client where
  sing = SClient

instance SingI Server where
  sing = SServer

data PingPong
  = S0 Bool
  | S1
  | End

data SPingPong :: PingPong -> Type where
  SS0 :: SPingPong (S0 b)
  SS1 :: SPingPong S1
  SEnd :: SPingPong End

type instance Sing = SPingPong

instance SingI (S0 b) where
  sing = SS0

instance SingI S1 where
  sing = SS1

instance SingI End where
  sing = SEnd

instance SingToInt Role where
  singToInt x = I# (dataToTag# x)

instance SingToInt PingPong where
  singToInt x = I# (dataToTag# x)

instance Protocol Role PingPong where
  type Done Client = End
  type Done Server = End
  data Msg Role PingPong from send recv where
    Ping :: Int -> Msg Role PingPong (S0 True) '(Client, S1) '(Server, S1)
    Pong :: Int -> Msg Role PingPong S1 '(Server, S0 s) '(Client, S0 s)
    Stop :: Msg Role PingPong (S0 False) '(Client, End) '(Server, End)

encodeMsg :: Encode Role PingPong L.ByteString
encodeMsg = Encode $ \x -> runPut $ case x of
  Ping i -> putWord8 0 >> put i
  Pong i -> putWord8 1 >> put i
  Stop -> putWord8 2

getAnyMsg :: Get (AnyMsg Role PingPong)
getAnyMsg = do
  v <- getWord8
  case v of
    0 -> get >>= pure . AnyMsg . Ping
    1 -> get >>= pure . AnyMsg . Pong
    2 -> pure (AnyMsg Stop)
    i -> error $ "undefined index: " ++ show i

convertDecoderLBS1 ::
  Decoder a ->
  (DecodeStep L.ByteString CodecFailure a)
convertDecoderLBS1 = go
  where
    go :: Decoder a -> DecodeStep L.ByteString CodecFailure a
    go (Done tr _ a) = DecodeDone a (Just $ L.fromStrict tr)
    go (Fail _ _ e) = DecodeFail (CodecFailure e)
    go (Partial k) = DecodePartial $ \mbs -> case mbs of
      Nothing -> go (Partial k)
      Just _bs -> go (k $ fmap L.toStrict mbs)

decodeMsg ::
  DecodeStep
    L.ByteString
    CodecFailure
    (AnyMsg Role PingPong)
decodeMsg = convertDecoderLBS1 (runGetIncremental getAnyMsg)

socketAsChannel :: Socket.Socket -> Channel IO L.ByteString
socketAsChannel socket =
  Channel {send, recv}
  where
    send :: L.ByteString -> IO ()
    send chunks =
      Socket.sendMany socket (L.toChunks chunks)

    recv :: IO (Maybe L.ByteString)
    recv = do
      chunk <- Socket.recv socket L.smallChunkSize
      if BS.null chunk
        then return Nothing
        else return (Just (L.fromStrict chunk))

instance Show (AnyMsg Role PingPong) where
  show (AnyMsg msg) = case msg of
    Ping i -> "Ping " <> show i
    Pong i -> "Pong " <> show i
    Stop -> "Stop"

myTracer :: (MonadSay m) => String -> Tracer Role PingPong m
myTracer st v = say (st <> show v)

data Choice :: PingPong -> Type where
  ST :: Choice (S0 True)
  SF :: Choice (S0 False)

choice :: (Monad m) => Int -> Peer Role PingPong Client m Choice (S0 s)
choice i =
  if i <= 5000
    then LiftM $ pure (ireturn ST)
    else LiftM $ pure (ireturn SF)

clientPeer ::
  (Monad m) => Int -> Peer Role PingPong Client m (At () (Done Client)) (S0 s)
clientPeer i = I.do
  res <- choice i
  case res of
    ST -> I.do
      yield (Ping i)
      Recv (Pong i') <- await
      clientPeer i'
    SF -> yield Stop

serverPeer ::
  (Monad m) => Peer Role PingPong Server m (At () (Done Server)) (S0 s)
serverPeer = I.do
  Recv msg <- await
  case msg of
    Ping i -> I.do
      yield (Pong (i + 1))
      serverPeer
    Stop -> returnAt ()
