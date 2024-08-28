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
{-# LANGUAGE NumericUnderscores #-}

{- |
Schematic diagram of the communication structure of three roles through typed-session:

<<data/fm.png>>

Some explanations for this diagram:

1. Roles are connected through channels, and there are many types of channels, such as channels established through TCP or channels established through TMVar.

2. Each role has a Peer thread, in which the Peer runs.

3. Each role has one or more decode threads, and the decoded Msgs are placed in the MsgCache.

4. SendMap aggregates the send functions of multiple Channels together.
When sending a message, the send function of the receiver is searched from SendMap.
-}
module TypedSession.Driver where

import Control.Concurrent.Class.MonadSTM
import Control.Monad.Class.MonadThrow (MonadThrow, throwIO)
import Control.Monad.Class.MonadTimer (MonadDelay, threadDelay)
import Data.IFunctor (At (..), Sing, SingI (sing))
import qualified Data.IFunctor as I
import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import GHC.Exception (Exception)
import TypedSession.Codec
import TypedSession.Core
import Unsafe.Coerce (unsafeCoerce)

{- |
Contains two functions sendMsg, recvMsg.
runPeerWithDriver uses them to send and receive Msg.
-}
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

{- |
Interpret Peer.
-}
runPeerWithDriver
  :: forall role' ps (r :: role') (st :: ps) m a
   . ( Monad m
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

{- |
A wrapper around AnyMsg that represents sending and receiving Msg.
-}
data TraceSendRecv role' ps where
  TraceSendMsg :: AnyMsg role' ps -> TraceSendRecv role' ps
  TraceRecvMsg :: AnyMsg role' ps -> TraceSendRecv role' ps

instance (Show (AnyMsg role' ps)) => Show (TraceSendRecv role' ps) where
  show (TraceSendMsg msg) = "Send " ++ show msg
  show (TraceRecvMsg msg) = "Recv " ++ show msg

{- |
Similar to the log function, used to print received or sent messages.
-}
type Tracer role' ps m = TraceSendRecv role' ps -> m ()

{- |
The default trace function. It simply ignores everything.
-}
nullTracer :: (Monad m) => a -> m ()
nullTracer _ = pure ()

{- |
SendMap aggregates the send functions of multiple Channels together.
When sending a message, the send function of the receiver is found from SendMap.
-}
type SendMap role' m bytes = IntMap (bytes -> m ())

{- |

Build Driver through SendMap and MsgCache.
Here we need some help from other functions:

1. `Tracer role' ps n` is similar to the log function, used to print received or sent messages.
2. `Encode role' ps` bytes encoding function, converts Msg into bytes.
3. `forall a. n a -> m a` This is a bit complicated, I will explain it in detail below.

I see Peer as three layers:

1. `Peer` upper layer, meets the requirements of McBride Indexed Monad, uses do syntax construction, has semantic checks, and is interpreted to the second layer m through runPeerWithDriver.
2. `m` middle layer, describes the business requirements in this layer, and converts the received Msg into specific business actions.
3. `n` bottom layer, responsible for receiving and sending bytes. It can have multiple options such as IO or IOSim. Using IOSim can easily test the code.
-}
driverSimple
  :: forall role' ps bytes m n
   . ( Monad m
     , Monad n
     , MonadSTM n
     )
  => Tracer role' ps n
  -> Encode role' ps bytes
  -> SendMap role' n bytes
  -> TVar n (MsgCache role' ps)
  -> (forall a. n a -> m a)
  -> Driver role' ps m
driverSimple tracer Encode{encode} sendMap tvar liftFun =
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
    case IntMap.lookup (singToInt role) sendMap of
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
    liftFun $ do
      anyMsg <- atomically $ do
        agencyMsg <- readTVar tvar
        case IntMap.lookup singInt agencyMsg of
          Nothing -> retry
          Just v -> do
            writeTVar tvar (IntMap.delete singInt agencyMsg)
            pure v
      tracer (TraceRecvMsg (anyMsg))
      pure anyMsg

{- |
decode loop, usually in a separate thread.

The decoded Msg is placed in MsgCache.

@
data Msg role' ps (from :: ps) (sendAndSt :: (role', ps)) (recvAndSt :: (role', ps))
@
Note that when placing a new Msg in MsgCache, if a Msg with the same `from` already exists in MsgCache, the decoding process will be blocked,
until that Msg is consumed before placing the new Msg in MsgCache.

This usually happens when the efficiency of Msg generation is greater than the efficiency of consumption.
-}
decodeLoop
  :: (Exception failure, MonadDelay n, MonadSTM n, MonadThrow n)
  => Tracer role' ps n
  -> Maybe bytes
  -> Decode role' ps failure bytes
  -> Channel n bytes
  -> TVar n (MsgCache role' ps)
  -> n ()
decodeLoop tracer mbt d@Decode{decode} channel tvar = do
  result <- runDecoderWithChannel channel mbt decode
  case result of
    Right (AnyMsg msg, mbt') -> do
      let agencyInt = singToInt $ msgFromStSing msg
      atomically $ do
        agencyMsg <- readTVar tvar
        case IntMap.lookup agencyInt agencyMsg of
          Nothing -> writeTVar tvar (IntMap.insert agencyInt (AnyMsg msg) agencyMsg)
          Just _v -> retry
      decodeLoop tracer mbt' d channel tvar
    Left failure -> do
      threadDelay 1_000_000
      throwIO failure
