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
{-# OPTIONS_GHC -Wno-unused-do-bind #-}

module PingPong where

import Control.Concurrent.Class.MonadSTM
import Control.Monad
import Control.Monad.Class.MonadFork (MonadFork, forkIO)
import Control.Monad.Class.MonadSay
import Control.Monad.Class.MonadThrow (MonadThrow)
import Data.IFunctor (At (..), Sing, SingI, ireturn, returnAt)
import qualified Data.IFunctor as I
import qualified Data.IntMap as IntMap
import Data.Kind
import GHC.Exts (dataToTag#)
import GHC.Int (Int (I#))
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

encodeMsg :: Encode Role PingPong (AnyMsg Role PingPong)
encodeMsg = Encode $ \x -> case x of
  Ping{} -> AnyMsg x
  Pong{} -> AnyMsg x
  Stop{} -> AnyMsg x

decodeMsg
  :: forall m
   . (Monad m)
  => DecodeStep
      (AnyMsg Role PingPong)
      CodecFailure
      m
      (AnyMsg Role PingPong)
decodeMsg =
  DecodePartial $ \case
    Nothing -> return $ DecodeFail (CodecFailure "expected more data")
    Just anyMsg -> pure $ DecodeDone anyMsg Nothing

data Choice :: PingPong -> Type where
  ST :: Choice (S0 True)
  SF :: Choice (S0 False)

choice :: (Monad m) => Int -> Peer Role PingPong Client m Choice (S0 s)
choice i =
  if i <= 5
    then LiftM $ pure (ireturn ST)
    else LiftM $ pure (ireturn SF)

clientPeer
  :: (Monad m) => Int -> Peer Role PingPong Client m (At () (Done Client)) (S0 s)
clientPeer i = I.do
  res <- choice i
  case res of
    ST -> I.do
      yield (Ping i)
      Recv (Pong i') <- await
      clientPeer i'
    SF -> yield Stop

serverPeer
  :: (Monad m) => Peer Role PingPong Server m (At () (Done Server)) (S0 s)
serverPeer = I.do
  Recv msg <- await
  case msg of
    Ping i -> I.do
      yield (Pong (i + 1))
      serverPeer
    Stop -> returnAt ()

mvarsAsChannel
  :: (MonadSTM m)
  => TMVar m a
  -> TMVar m a
  -> Channel m a
mvarsAsChannel bufferRead bufferWrite =
  Channel{send, recv}
 where
  send x = atomically (putTMVar bufferWrite x)
  recv = atomically (Just <$> takeTMVar bufferRead)

myTracer :: (MonadSay m) => String -> Tracer Role PingPong m
myTracer st v = say (st <> show v)

instance Show (AnyMsg Role PingPong) where
  show (AnyMsg msg) = case msg of
    Ping i -> "Ping " <> show i
    Pong i -> "Pong " <> show i
    Stop -> "Stop"

runAll :: forall m. (Monad m, MonadSTM m, MonadSay m, MonadFork m, MonadThrow m) => m ()
runAll = do
  clientTMVar <- newEmptyTMVarIO @m @(AnyMsg Role PingPong)
  serverTMVar <- newEmptyTMVarIO @m @(AnyMsg Role PingPong)
  let clientChannel = mvarsAsChannel @m clientTMVar serverTMVar
      serverChannel = mvarsAsChannel @m serverTMVar clientTMVar
      sendFun bufferWrite x = atomically (putTMVar bufferWrite x)
      sendToRole =
        IntMap.fromList
          [ (singToInt SServer, sendFun serverTMVar)
          , (singToInt SClient, sendFun clientTMVar)
          ]
  clientTvar <- newTVarIO IntMap.empty
  serverTvar <- newTVarIO IntMap.empty
  let clientDriver = driverSimple (myTracer "client: ") encodeMsg sendToRole clientTvar
      serverDriver = driverSimple (myTracer "server: ") encodeMsg sendToRole serverTvar
  -- fork client decode thread
  forkIO $ decodeLoop (myTracer "client: ") Nothing (Decode decodeMsg) clientChannel clientTvar
  -- fork server decode thread
  forkIO $ decodeLoop (myTracer "server: ") Nothing (Decode decodeMsg) serverChannel serverTvar
  -- fork server Peer thread
  forkIO $ void $ runPeerWithDriver serverDriver serverPeer
  -- run client Peer
  void $ runPeerWithDriver clientDriver (clientPeer 0)
