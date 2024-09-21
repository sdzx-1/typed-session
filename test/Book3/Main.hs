{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE QualifiedDo #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}

module Book3.Main where

import Book3.Peer
import Book3.Protocol
import Book3.Type
import Control.Carrier.Lift (runM, sendM)
import Control.Carrier.Random.Gen (runRandom)
import Control.Concurrent.Class.MonadSTM
import Control.Monad
import Control.Monad.Class.MonadFork (MonadFork, forkIO)
import Control.Monad.Class.MonadSay
import Control.Monad.Class.MonadThrow (MonadThrow)
import Control.Monad.Class.MonadTimer (MonadDelay)
import Control.Monad.IOSim
import System.Random (StdGen, split)
import TypedSession.Codec
import TypedSession.Core
import TypedSession.Driver

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

myTracer :: (MonadSay m) => String -> Tracer BookRole Book m
myTracer st v = say (st <> show v)

runAll
  :: forall n
   . ( Monad n
     , MonadSTM n
     , MonadSay n
     , MonadThrow n
     , MonadFork n
     , MonadDelay n
     )
  => StdGen
  -> n ()
runAll g = do
  buyerTMVar <- newEmptyTMVarIO @n @(AnyMsg BookRole Book)
  buyer2TMVar <- newEmptyTMVarIO @n @(AnyMsg BookRole Book)
  sellerTMVar <- newEmptyTMVarIO @n @(AnyMsg BookRole Book)

  buyerDriver <-
    driverSimple
      (myTracer "buyer :")
      encodeMsg
      (Decode decodeMsg)
      [ (SomeRole SSeller, mvarsAsChannel buyerTMVar sellerTMVar)
      , (SomeRole SBuyer2, mvarsAsChannel buyerTMVar buyer2TMVar)
      ]
      sendM

  buyer2Driver <-
    driverSimple
      (myTracer "buyer2 :")
      encodeMsg
      (Decode decodeMsg)
      [(SomeRole SBuyer, mvarsAsChannel buyer2TMVar buyerTMVar)]
      sendM

  sellerDriver <-
    driverSimple
      (myTracer "seller :")
      encodeMsg
      (Decode decodeMsg)
      [(SomeRole SBuyer, mvarsAsChannel sellerTMVar buyerTMVar)]
      sendM

  let (g0, g1) = split g
      (g2, g3) = split g0

  resultTMVar1 <- newEmptyTMVarIO
  resultTMVar2 <- newEmptyTMVarIO

  -- fork seller Peer thread
  forkIO $ do
    runM $ runRandom g1 $ runPeerWithDriver sellerDriver sellerPeer
    atomically $ writeTMVar resultTMVar1 ()

  -- fork buyer2 Peer thread
  forkIO $ do
    runM $ runRandom g2 $ runPeerWithDriver buyer2Driver buyer2Peer
    atomically $ writeTMVar resultTMVar2 ()

  -- run buyer Peer
  void $ runM $ runRandom g3 $ runPeerWithDriver buyerDriver buyerPeer

  -- wait seller, buyer
  atomically $ do
    takeTMVar resultTMVar1
    takeTMVar resultTMVar2

book3Prop :: StdGen -> Either Failure ()
book3Prop v = runSim (runAll v)