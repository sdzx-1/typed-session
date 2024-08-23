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

module PingPong.Peer where

import Control.Monad.Class.MonadSay
import Control.Monad.Class.MonadTimer (threadDelay)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Binary
import Data.Binary.Get
import Data.Binary.Put
import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder.Extra as L
import qualified Data.ByteString.Lazy as L
import Data.IFunctor (At (..), ireturn, returnAt)
import qualified Data.IFunctor as I
import Data.Kind
import qualified Network.Socket as Socket
import qualified Network.Socket.ByteString as Socket
import PingPong.Protocol
import PingPong.Type
import TypedSession.Codec
import TypedSession.Core
import TypedSession.Driver

encodeMsg :: Encode PingPongRole PingPong L.ByteString
encodeMsg = Encode $ \x -> runPut $ case x of
  Ping -> putWord8 0
  Pong -> putWord8 1
  Stop -> putWord8 2
  AddOne -> putWord8 3
  CStop -> putWord8 4
  Recved -> putWord8 5

getAnyMsg :: Get (AnyMsg PingPongRole PingPong)
getAnyMsg = do
  v <- getWord8
  case v of
    0 -> pure (AnyMsg Ping)
    1 -> pure (AnyMsg Pong)
    2 -> pure (AnyMsg Stop)
    3 -> pure (AnyMsg AddOne)
    4 -> pure (AnyMsg CStop)
    5 -> pure (AnyMsg Recved)
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
      (AnyMsg PingPongRole PingPong)
decodeMsg = convertDecoderLBS1 (runGetIncremental getAnyMsg)

socketAsChannel :: Socket.Socket -> Channel IO L.ByteString
socketAsChannel socket =
  Channel{send, recv}
 where
  send :: L.ByteString -> IO ()
  send chunks = do
    threadDelay 50000
    Socket.sendMany socket (L.toChunks chunks)

  recv :: IO (Maybe L.ByteString)
  recv = do
    chunk <- Socket.recv socket L.smallChunkSize
    if BS.null chunk
      then return Nothing
      else return (Just (L.fromStrict chunk))

myTracer :: (MonadSay m) => String -> Tracer PingPongRole PingPong m
myTracer st v = say (st <> show v)

data Choice :: PingPong -> Type where
  ST :: Choice (S2 STrue)
  SF :: Choice (S1 SFalse)

choice :: (Monad m) => Int -> Peer PingPongRole PingPong Client m Choice S0
choice i =
  if i <= 5
    then LiftM $ pure (ireturn ST)
    else LiftM $ pure (ireturn SF)

clientPeer
  :: (Monad m) => Int -> Peer PingPongRole PingPong Client m (At () (Done Client)) S0
clientPeer i = I.do
  res <- choice i
  case res of
    ST -> I.do
      yield AddOne
      Recv Recved <- await
      yield Ping
      Recv Pong <- await
      clientPeer (i + 1)
    SF -> I.do
      yield Stop
      yield CStop

serverPeer
  :: (Monad m) => Peer PingPongRole PingPong Server m (At () (Done Server)) (S1 s)
serverPeer = I.do
  Recv msg <- await
  case msg of
    Ping -> I.do
      yield Pong
      serverPeer
    Stop -> returnAt ()

counterPeer
  :: (MonadIO m) => Int -> Peer PingPongRole PingPong Counter m (At () (Done Server)) (S2 s)
counterPeer i = I.do
  Recv msg <- await
  case msg of
    AddOne -> I.do
      liftm $ liftIO $ putStrLn $ "counter val: " <> show i
      yield Recved
      counterPeer (i + 1)
    CStop -> returnAt ()