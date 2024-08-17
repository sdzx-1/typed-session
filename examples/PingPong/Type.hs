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
import Control.Monad.Class.MonadTimer (threadDelay)
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
import TypedSession.Codec
import TypedSession.Core
import TypedSession.Driver

{-

-----------------------------------------------Client------------------------Server-----------------------Counter
LABEL 0                                         S0 s                          S0 s                          S1 s
  [Branch] Client                               S0 s                          S0 s                          S1 s
    * BranchSt True
    Ping                                     S0 True->                       ->S0 s                         S1 s
    Pong                                        S2<-                          <-S2                          S1 s
    AddOne                                   S1 True->                        S0 s                         ->S1 s
    ^ Goto 0                                    S0 s                          S0 s                          S1 s
    * BranchSt False
    Stop                                     S0 False->                      ->S0 s                         S1 s
    CStop                                    S1 False->                       End                          ->S1 s
    ~ Terminal                                  End                           End                           End

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
instance SingToInt Role where
  singToInt x = I# (dataToTag# x)
data PingPongSt
  = End
  | S0 Bool
  | S1 Bool
  | S2
data SPingPongSt :: PingPongSt -> Type where
  SEnd :: SPingPongSt End
  SS0 :: SPingPongSt (S0 s)
  SS1 :: SPingPongSt (S1 s)
  SS2 :: SPingPongSt S2
type instance Sing = SPingPongSt
instance SingI End where
  sing = SEnd
instance SingI (S0 s) where
  sing = SS0
instance SingI (S1 s) where
  sing = SS1
instance SingI S2 where
  sing = SS2
instance SingToInt PingPongSt where
  singToInt x = I# (dataToTag# x)
instance Protocol Role PingPongSt where
  type Done Client = End
  type Done Server = End
  type Done Counter = End
  data Msg Role PingPongSt from send recv where
    Ping :: Msg Role PingPongSt (S0 True) '(Client, S2) '(Server, S2)
    Pong :: Msg Role PingPongSt (S2) '(Server, S0 s) '(Client, S1 True)
    AddOne :: Msg Role PingPongSt (S1 True) '(Client, S0 s) '(Counter, S1 s)
    Stop :: Msg Role PingPongSt (S0 False) '(Client, S1 False) '(Server, End)
    CStop :: Msg Role PingPongSt (S1 False) '(Client, End) '(Counter, End)

encodeMsg :: Encode Role PingPongSt L.ByteString
encodeMsg = Encode $ \x -> runPut $ case x of
  Ping -> putWord8 0
  Pong -> putWord8 1
  Stop -> putWord8 2
  AddOne -> putWord8 3
  CStop -> putWord8 4

getAnyMsg :: Get (AnyMsg Role PingPongSt)
getAnyMsg = do
  v <- getWord8
  case v of
    0 -> pure (AnyMsg Ping)
    1 -> pure (AnyMsg Pong)
    2 -> pure (AnyMsg Stop)
    3 -> pure (AnyMsg AddOne)
    4 -> pure (AnyMsg CStop)
    i -> error $ "undefined index: " ++ show i

convertDecoderLBS1
  :: Decoder a
  -> (DecodeStep L.ByteString CodecFailure a)
convertDecoderLBS1 = go
 where
  go :: Decoder a -> DecodeStep L.ByteString CodecFailure a
  go (Done tr _ a) = DecodeDone a (Just $ L.fromStrict tr)
  go (Fail _ _ e) = DecodeFail (CodecFailure e)
  go (Partial k) = DecodePartial $ \mbs -> case mbs of
    Nothing -> go (Partial k)
    Just _bs -> go (k $ fmap L.toStrict mbs)

decodeMsg
  :: DecodeStep
      L.ByteString
      CodecFailure
      (AnyMsg Role PingPongSt)
decodeMsg = convertDecoderLBS1 (runGetIncremental getAnyMsg)

socketAsChannel :: Socket.Socket -> Channel IO L.ByteString
socketAsChannel socket =
  Channel{send, recv}
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

instance Show (AnyMsg Role PingPongSt) where
  show (AnyMsg msg) = case msg of
    Ping -> "Ping"
    Pong -> "Pong"
    Stop -> "Stop"
    AddOne -> "AddOne"
    CStop -> "CStop"

myTracer :: (MonadSay m) => String -> Tracer Role PingPongSt m
myTracer st v = say (st <> show v)

data Choice :: PingPongSt -> Type where
  ST :: Choice (S0 True)
  SF :: Choice (S0 False)

choice :: (Monad m) => Int -> Peer Role PingPongSt Client m Choice (S0 s)
choice i =
  if i <= 5
    then LiftM $ pure (ireturn ST)
    else LiftM $ pure (ireturn SF)

clientPeer
  :: (Monad m) => Int -> Peer Role PingPongSt Client m (At () (Done Client)) (S0 s)
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

serverPeer
  :: (Monad m) => Peer Role PingPongSt Server m (At () (Done Server)) (S0 s)
serverPeer = I.do
  Recv msg <- await
  case msg of
    Ping -> I.do
      yield Pong
      serverPeer
    Stop -> returnAt ()

counterPeer
  :: (MonadIO m) => Int -> Peer Role PingPongSt Counter m (At () (Done Server)) (S1 s)
counterPeer i = I.do
  Recv msg <- await
  case msg of
    AddOne -> I.do
      liftm $ liftIO $ putStrLn $ "counter val: " <> show i
      counterPeer (i + 1)
    CStop -> returnAt ()