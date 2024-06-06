{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE QualifiedDo #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}

module Type where

import Control.Monad.Class.MonadSay
import Control.Monad.IO.Class (MonadIO, liftIO)
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
import Control.Monad.Class.MonadTimer (threadDelay)

{-

-----------------Client--------------Server-------------Counter-----------------
                 (S0 s)              (S0 s)              (S2 s)
    ---------------------------------[True]---------------------------------
              (S0 '[True])           (S0 s)              (S2 s)
        Ping       |       ----->      |
                   S1                  S1                (S2 s)
        Pong       |       <-----      |
              (S2 '[True])            S0 s               (S2 s)
       AddOne      |                 ----->                |
                  S0 s                S0 s                S2 s

    --------------------------------[False]---------------------------------
             (S0 '[False])           (S0 s)              (S2 s)
        Stop       |       ----->      |
             (S2 '[False])            End                (S2 s)
       CStop       |                 ----->                |
                  End                 End                 End
                                    Terminal
 -}

data Role = Client | Server | Counter
  deriving (Show, Eq, Ord)

data SRole :: Role -> Type where
  SClient :: SRole Client
  SServer :: SRole Server
  SCounter :: SRole Counter

type instance Sing = SRole

instance SingI Client where
  sing = SClient

instance SingI Server where
  sing = SServer

instance SingI Counter where
  sing = SCounter

data PingPong
  = S0 [Bool]
  | S1
  | S2 [Bool]
  | End

data SPingPong :: PingPong -> Type where
  SS0 :: SPingPong (S0 b)
  SS1 :: SPingPong S1
  SS2 :: SPingPong (S2 b)
  SEnd :: SPingPong End

type instance Sing = SPingPong

instance SingI (S0 b) where
  sing = SS0

instance SingI S1 where
  sing = SS1

instance SingI (S2 b) where
  sing = SS2

instance SingI End where
  sing = SEnd

instance SingToInt Role where
  singToInt x = I# (dataToTag# x)

instance SingToInt PingPong where
  singToInt x = I# (dataToTag# x)

instance Protocol Role PingPong where
  type Done Client = End
  type Done Server = End
  type Done Counter = End
  data Msg Role PingPong from send recv where
    Ping :: Msg Role PingPong (S0 '[True]) '(Client, S1) '(Server, S1)
    Pong :: Msg Role PingPong S1 '(Server, S0 s) '(Client, S2 '[True])
    Stop :: Msg Role PingPong (S0 '[False]) '(Client, S2 '[False]) '(Server, End)
    AddOne :: Msg Role PingPong (S2 '[True]) '(Client, S0 s) '(Counter, S2 s)
    CStop :: Msg Role PingPong (S2 '[False]) '(Client, End) '(Counter, End)

encodeMsg :: Encode Role PingPong L.ByteString
encodeMsg = Encode $ \x -> runPut $ case x of
  Ping -> putWord8 0
  Pong -> putWord8 1
  Stop -> putWord8 2
  AddOne -> putWord8 3
  CStop -> putWord8 4

getAnyMsg :: Get (AnyMsg Role PingPong)
getAnyMsg = do
  v <- getWord8
  case v of
    0 -> pure (AnyMsg Ping)
    1 -> pure (AnyMsg Pong)
    2 -> pure (AnyMsg Stop)
    3 -> pure (AnyMsg AddOne)
    4 -> pure (AnyMsg CStop)
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
    send chunks = do 
      threadDelay 500000
      Socket.sendMany socket (L.toChunks chunks)

    recv :: IO (Maybe L.ByteString)
    recv = do
      chunk <- Socket.recv socket L.smallChunkSize
      if BS.null chunk
        then return Nothing
        else return (Just (L.fromStrict chunk))

instance Show (AnyMsg Role PingPong) where
  show (AnyMsg msg) = case msg of
    Ping -> "Ping"
    Pong -> "Pong"
    Stop -> "Stop"
    AddOne -> "AddOne"
    CStop -> "CStop"

myTracer :: (MonadSay m) => String -> Tracer Role PingPong m
myTracer st v = say (st <> show v)

data Choice :: PingPong -> Type where
  ST :: Choice (S0 '[True])
  SF :: Choice (S0 '[False])

choice :: (Monad m) => Int -> Peer Role PingPong Client m Choice (S0 s)
choice i =
  if i <= 5
    then LiftM $ pure (ireturn ST)
    else LiftM $ pure (ireturn SF)

clientPeer ::
  (Monad m) => Int -> Peer Role PingPong Client m (At () (Done Client)) (S0 s)
clientPeer i = I.do
  res <- choice i
  case res of
    ST -> I.do
      yield Ping
      Recv Pong <- await
      yield AddOne
      clientPeer (i + 1)
    SF -> I.do
      yield Stop
      yield CStop

serverPeer ::
  (Monad m) => Peer Role PingPong Server m (At () (Done Server)) (S0 s)
serverPeer = I.do
  Recv msg <- await
  case msg of
    Ping -> I.do
      yield Pong
      serverPeer
    Stop -> returnAt ()

counterPeer ::
  (MonadIO m) => Int -> Peer Role PingPong Counter m (At () (Done Server)) (S2 s)
counterPeer i = I.do
  Recv msg <- await
  case msg of
    AddOne -> I.do
      liftm $ liftIO $ putStrLn $ "counter val: " <> show i
      counterPeer (i + 1)
    CStop -> returnAt ()