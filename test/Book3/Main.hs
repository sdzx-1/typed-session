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
import Control.Carrier.Random.Gen (runRandom)
import Control.Concurrent.Class.MonadSTM
import Control.Effect.Labelled (runLabelledLift, sendM)
import Control.Monad
import Control.Monad.Class.MonadFork (MonadFork, forkIO)
import Control.Monad.Class.MonadSay
import Control.Monad.Class.MonadThrow (MonadThrow)
import Control.Monad.IOSim
import qualified Data.IntMap as IntMap
import System.Random (StdGen, split)
import TypedSession.Codec
import TypedSession.Core
import TypedSession.Driver
import Control.Monad.Class.MonadTimer (MonadDelay)

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
  let buyerSellerChannel = mvarsAsChannel @n buyerTMVar sellerTMVar
      buyerBuyer2Channel = mvarsAsChannel @n buyerTMVar buyer2TMVar

      sellerBuyerChannel = mvarsAsChannel @n sellerTMVar buyerTMVar

      buyer2BuyerChannel = mvarsAsChannel @n buyer2TMVar buyerTMVar

      sendFun bufferWrite x = atomically @n (putTMVar bufferWrite x)
      sendToRole =
        IntMap.fromList
          [ (singToInt SSeller, sendFun sellerTMVar)
          , (singToInt SBuyer, sendFun buyerTMVar)
          , (singToInt SBuyer2, sendFun buyer2TMVar)
          ]
  buyerTvar <- newTVarIO IntMap.empty
  buyer2Tvar <- newTVarIO IntMap.empty
  sellerTvar <- newTVarIO IntMap.empty
  let buyerDriver = driverSimple (myTracer "buyer :") encodeMsg sendToRole buyerTvar sendM
      buyer2Driver = driverSimple (myTracer "buyer2 :") encodeMsg sendToRole buyer2Tvar sendM
      sellerDriver = driverSimple (myTracer "seller :") encodeMsg sendToRole sellerTvar sendM
  -- fork buyer decode thread, seller -> buyer
  forkIO $ decodeLoop (myTracer "buyer :") Nothing (Decode decodeMsg) buyerSellerChannel buyerTvar
  -- fork buyer decode thread, buyer2 -> buyer
  forkIO $ decodeLoop (myTracer "buyer :") Nothing (Decode decodeMsg) buyerBuyer2Channel buyerTvar

  -- fork seller decode thread, buyer -> seller
  forkIO $ decodeLoop (myTracer "seller :") Nothing (Decode decodeMsg) sellerBuyerChannel sellerTvar

  -- fork buyer2 decode thread, buyer -> buyer2
  forkIO $ decodeLoop (myTracer "buyer2 :") Nothing (Decode decodeMsg) buyer2BuyerChannel buyer2Tvar

  let (g0, g1) = split g
      (g2, g3) = split g0

  resultTMVar1 <- newEmptyTMVarIO
  resultTMVar2 <- newEmptyTMVarIO

  -- fork seller Peer thread
  forkIO $ do
    runLabelledLift $ runRandom g1 $ runPeerWithDriver sellerDriver sellerPeer
    atomically $ writeTMVar resultTMVar1 ()

  -- fork buyer2 Peer thread
  forkIO $ do
    runLabelledLift $ runRandom g2 $ runPeerWithDriver buyer2Driver buyer2Peer
    atomically $ writeTMVar resultTMVar2 ()

  -- run buyer Peer
  void $ runLabelledLift $ runRandom g3 $ runPeerWithDriver buyerDriver buyerPeer

  -- wait seller, buyer
  atomically $ do
    takeTMVar resultTMVar1
    takeTMVar resultTMVar2

book3Prop :: StdGen -> Either Failure ()
book3Prop v = runSim (runAll v)