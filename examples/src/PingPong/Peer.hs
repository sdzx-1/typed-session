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
  Check i -> putWord8 6 >> put i
  CheckResult i -> putWord8 7 >> put i

getAnyMsg :: Get (AnyMsg PingPongRole PingPong)
getAnyMsg = do
  v <- getWord8
  case v of
    0 -> pure (AnyMsg Ping)
    1 -> pure (AnyMsg Pong)
    2 -> pure (AnyMsg Stop)
    3 -> pure (AnyMsg AddOne)
    4 -> pure (AnyMsg CStop)
    6 -> do
      i <- get
      pure (AnyMsg $ Check i)
    7 -> do
      i <- get
      pure (AnyMsg $ CheckResult i)
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
    Nothing -> DecodeFail (CodecFailure "Peer disconnected!!")
    Just bs -> go (k $ Just $ L.toStrict bs)

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
    Socket.sendMany socket (L.toChunks chunks)

  recv :: IO (Maybe L.ByteString)
  recv = do
    chunk <- Socket.recv socket L.smallChunkSize
    if BS.null chunk
      then return Nothing
      else return (Just (L.fromStrict chunk))

myTracer :: (MonadSay m) => String -> Tracer PingPongRole PingPong m
myTracer st v = say (st <> show v)

choice :: (Monad m) => Int -> ChoiceNextActionFun m
choice i =
  if i `mod` 10 == 1
    then liftConstructor BranchSt_CheckVal
    else
      if i <= 50
        then liftConstructor BranchSt_Continue
        else liftConstructor BranchSt_Finish

clientPeer
  :: (Monad m) => Int -> Peer PingPongRole PingPong Client m (At () (Done Client)) ClientStartSt
clientPeer i = I.do
  res <- choice i
  case res of
    BranchSt_CheckVal -> I.do
      yield (Check i)
      (CheckResult _b) <- await
      clientPeer (i + 1)
    BranchSt_Continue -> I.do
      yield Ping
      Pong <- await
      yield AddOne
      clientPeer (i + 1)
    BranchSt_Finish -> I.do
      yield Stop
      yield CStop

serverPeer
  :: (Monad m) => Peer PingPongRole PingPong Server m (At () (Done Server)) (ServerStartSt s)
serverPeer = I.do
  msg <- await
  case msg of
    Ping -> I.do
      yield Pong
      serverPeer
    Stop -> returnAt ()

counterPeer
  :: (MonadIO m) => Int -> Peer PingPongRole PingPong Counter m (At () (Done Server)) (CounterStartSt s)
counterPeer i = I.do
  msg <- await
  case msg of
    Check _i -> I.do
      yield (CheckResult True)
      counterPeer i
    AddOne -> I.do
      liftm $ liftIO $ putStrLn $ "counter val: " <> show i
      counterPeer (i + 1)
    CStop -> returnAt ()