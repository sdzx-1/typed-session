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

import Control.Concurrent.Class.MonadSTM
import Control.Monad.Class.MonadThrow (MonadThrow, throwIO)
import Data.IFunctor (At (..), Sing, SingI (sing))
import qualified Data.IFunctor as I
import qualified Data.IntMap as IntMap
import GHC.Exception (Exception)
import TypedProtocol.Codec
import TypedProtocol.Core
import Unsafe.Coerce (unsafeCoerce)

data Driver role' ps m
  = Driver
  { sendMsg
      :: forall (send :: role') (recv :: role') (st :: ps) (st' :: ps) (st'' :: ps)
       . ( SingI recv
         , SingI st
         , SingToInt ps
         , SingToInt role'
         )
      => Sing recv
      -> Msg role' ps st '(send, st') '(recv, st'')
      -> m ()
  , recvMsg
      :: forall (st' :: ps)
       . (SingToInt ps)
      => Sing st'
      -> m (AnyMsg role' ps)
  }

runPeerWithDriver
  :: forall role' ps (r :: role') (st :: ps) m a
   . ( Monad m
     , MonadSTM m
     , (SingToInt role')
     )
  => Driver role' ps m
  -> Peer role' ps r m (At a (Done r)) st
  -> m a
runPeerWithDriver Driver{sendMsg, recvMsg} =
  go
 where
  go
    :: forall st'
     . Peer role' ps r m (At a (Done r)) st'
    -> m a
  go (IReturn (At a)) = pure a
  go (LiftM k) = k >>= go
  go (Yield (msg :: Msg role' ps (st' :: ps) '(r, sps) '(recv :: role', rps)) k) = do
    sendMsg (sing @recv) msg
    go k
  go (Await (k :: (Recv role' ps r st' I.~> Peer role' ps r m ia))) = do
    AnyMsg msg <- recvMsg (sing @st')
    go (k $ unsafeCoerce (Recv msg))

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
  :: forall role' ps bytes m n
   . ( Monad m
     , Monad n
     , MonadSTM n
     , Ord role'
     )
  => Tracer role' ps n
  -> Encode role' ps bytes
  -> SendToRole role' n bytes
  -> TVar n (AgencyMsg role' ps)
  -> (forall a. n a -> m a)
  -> Driver role' ps m
driverSimple tracer Encode{encode} sendToRole tvar liftFun =
  Driver{sendMsg, recvMsg}
 where
  sendMsg
    :: forall (send :: role') (recv :: role') (from :: ps) (st :: ps) (st1 :: ps)
     . ( SingI recv
       , SingI from
       , SingToInt ps
       , SingToInt role'
       )
    => Sing recv
    -> Msg role' ps from '(send, st) '(recv, st1)
    -> m ()
  sendMsg role msg = liftFun $ do
    case IntMap.lookup (singToInt role) sendToRole of
      Nothing -> error "np"
      Just sendFun -> sendFun (encode msg)
    tracer (TraceSendMsg (AnyMsg msg))

  recvMsg
    :: forall (st' :: ps)
     . (SingToInt ps)
    => Sing st'
    -> m (AnyMsg role' ps)
  recvMsg sst' = do
    let singInt = singToInt sst'
    liftFun $ atomically $ do
      agencyMsg <- readTVar tvar
      case IntMap.lookup singInt agencyMsg of
        Nothing -> retry
        Just v -> do
          writeTVar tvar (IntMap.delete singInt agencyMsg)
          pure v

decodeLoop
  :: (Exception failure, MonadSTM n, MonadThrow n)
  => Tracer role' ps n
  -> Maybe bytes
  -> Decode role' ps failure n bytes
  -> Channel n bytes
  -> TVar n (AgencyMsg role' ps)
  -> n ()
decodeLoop tracer mbt d@Decode{decode} channel tvar = do
  result <- runDecoderWithChannel channel mbt decode
  case result of
    Right (anyMsg@(AnyMsg msg), mbt') -> do
      tracer (TraceRecvMsg anyMsg)
      let agencyInt = singToInt $ msgFromStSing msg
      atomically $ do
        agencyMsg <- readTVar tvar
        case IntMap.lookup agencyInt agencyMsg of
          Nothing -> writeTVar tvar (IntMap.insert agencyInt (AnyMsg msg) agencyMsg)
          Just _v -> retry
      decodeLoop tracer mbt' d channel tvar
    Left failure -> throwIO failure
