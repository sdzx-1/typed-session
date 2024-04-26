--  This part of the code comes from typed-protocols, I modified a few things.
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}

module TypedProtocol.Driver where

import Control.Exception (throwIO)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.IFunctor (At (..), SingI (sing))
import qualified Data.IFunctor as I
import GHC.Exception (Exception)
import TypedProtocol.Codec
import TypedProtocol.Core

data Driver role' ps dstate m
  = Driver
  { sendMsg
      :: forall (send :: role') (recv :: role') (st :: ps) (st' :: ps) (st'' :: ps)
       . Agency role' ps recv st
      -> Msg role' ps st '(send, st') '(recv, st'')
      -> m ()
  , recvMsg
      :: forall (recv :: role') (from :: ps)
       . Agency role' ps recv from
      -> dstate
      -> m (SomeMsg role' ps recv from, dstate)
  , startDState :: dstate
  }

runPeerWithDriver
  :: forall role' ps (r :: role') (st :: ps) dstate m a
   . (Monad m)
  => Driver role' ps dstate m
  -> Peer role' ps r m (At a (Done r)) st
  -> dstate
  -> m (a, dstate)
runPeerWithDriver Driver{sendMsg, recvMsg} =
  flip go
 where
  go
    :: forall st'
     . dstate
    -> Peer role' ps r m (At a (Done r)) st'
    -> m (a, dstate)
  go dstate (IReturn (At a)) = pure (a, dstate)
  go dstate (LiftM k) = k >>= go dstate
  go dstate (Yield (msg :: Msg role' ps (st' :: ps) '(r, sps) '(recv :: role', rps)) k) = do
    sendMsg (Agency (sing @recv) (sing @st')) msg
    go dstate k
  go dstate (Await (k :: (Recv role' ps r st' I.~> Peer role' ps r m ia))) = do
    (SomeMsg msg, dstate') <- recvMsg (Agency (sing @r) (sing @st')) dstate
    go dstate' (k msg)

data AnyMsg role' ps where
  AnyMsg
    :: Msg role' ps st '(send, st') '(recv, st'')
    -> AnyMsg role' ps

data TraceSendRecv role' ps where
  TraceSendMsg :: AnyMsg role' ps -> TraceSendRecv role' ps
  TraceRecvMsg :: AnyMsg role' ps -> TraceSendRecv role' ps

instance (Show (AnyMsg role' ps)) => Show (TraceSendRecv role' ps) where
  show (TraceSendMsg msg) = "Send " ++ show msg
  show (TraceRecvMsg msg) = "Recv " ++ show msg

type Tracer role' ps m = TraceSendRecv role' ps -> m ()

nullTracer :: (Monad m) => a -> m ()
nullTracer _ = pure ()

driverSimple
  :: forall role' ps failure bytes m
   . (Monad m, MonadIO m, Exception failure, Ord role')
  => Tracer role' ps m
  -> Codec role' ps failure m bytes
  -> Channel role' m bytes
  -> Driver role' ps (Maybe bytes) m
driverSimple tracer Codec{encode, decode} channel@Channel{sendFun} =
  Driver{sendMsg, recvMsg, startDState = Nothing}
 where
  sendMsg
    :: forall (send :: role') (recv :: role') (from :: ps) (st :: ps) (st1 :: ps)
     . Agency role' ps recv from
    -> Msg role' ps from '(send, st) '(recv, st1)
    -> m ()
  sendMsg stok@(Agency srecv _) msg = do
    sendFun srecv (encode stok msg)
    tracer (TraceSendMsg (AnyMsg msg))

  recvMsg
    :: forall (recv :: role') (from :: ps)
     . (Monad m, MonadIO m)
    => Agency role' ps recv from
    -> Maybe bytes
    -> m (SomeMsg role' ps recv from, Maybe bytes)
  recvMsg stok trailing = do
    decoder <- decode stok
    result <- runDecoderWithChannel channel trailing decoder
    case result of
      Right x@(SomeMsg (Recv msg), _) -> do
        tracer (TraceRecvMsg (AnyMsg msg))
        pure x
      Left failure -> liftIO $ throwIO failure
