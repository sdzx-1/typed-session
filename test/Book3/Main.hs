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
import Book3.Type
import Control.Concurrent.Class.MonadSTM
import Control.Monad
import Control.Monad.Class.MonadFork (forkIO)
import Control.Monad.Class.MonadSay
import qualified Data.IntMap as IntMap
import TypedProtocol.Codec
import TypedProtocol.Core
import TypedProtocol.Driver

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

myTracer :: (MonadSay m) => String -> Tracer Role BookSt m
myTracer st v = say (st <> show v)

runAll :: IO ()
runAll = do
  buyerTMVar <- newEmptyTMVarIO @_ @(AnyMsg Role BookSt)
  buyer2TMVar <- newEmptyTMVarIO @_ @(AnyMsg Role BookSt)
  sellerTMVar <- newEmptyTMVarIO @_ @(AnyMsg Role BookSt)
  let buyerSellerChannel = mvarsAsChannel @_ buyerTMVar sellerTMVar
      buyerBuyer2Channel = mvarsAsChannel @_ buyerTMVar buyer2TMVar

      sellerBuyerChannel = mvarsAsChannel @_ sellerTMVar buyerTMVar

      buyer2BuyerChannel = mvarsAsChannel @_ buyer2TMVar buyerTMVar

      sendFun bufferWrite x = atomically (putTMVar bufferWrite x)
      sendToRole =
        IntMap.fromList
          [ (singToInt SSeller, sendFun sellerTMVar)
          , (singToInt SBuyer, sendFun buyerTMVar)
          , (singToInt SBuyer2, sendFun buyer2TMVar)
          ]
  buyerTvar <- newTVarIO IntMap.empty
  buyer2Tvar <- newTVarIO IntMap.empty
  sellerTvar <- newTVarIO IntMap.empty
  let buyerDriver = driverSimple (myTracer "buyer :") encodeMsg sendToRole buyerTvar
      buyer2Driver = driverSimple (myTracer "buyer2 :") encodeMsg sendToRole buyer2Tvar
      sellerDriver = driverSimple (myTracer "seller :") encodeMsg sendToRole sellerTvar
  -- fork buyer decode thread, seller -> buyer
  forkIO $ decodeLoop (myTracer "buyer :") Nothing (Decode decodeMsg) buyerSellerChannel buyerTvar
  -- fork buyer decode thread, buyer2 -> buyer
  forkIO $ decodeLoop (myTracer "buyer :") Nothing (Decode decodeMsg) buyerBuyer2Channel buyerTvar

  -- fork seller decode thread, buyer -> seller
  forkIO $ decodeLoop (myTracer "seller :") Nothing (Decode decodeMsg) sellerBuyerChannel sellerTvar

  -- fork buyer2 decode thread, buyer -> buyer2
  forkIO $ decodeLoop (myTracer "buyer2 :") Nothing (Decode decodeMsg) buyer2BuyerChannel buyer2Tvar

  -- fork seller Peer thread
  forkIO $ void $ runPeerWithDriver sellerDriver sellerPeer

  -- fork buyer2 Peer thread
  forkIO $ void $ runPeerWithDriver buyer2Driver buyer2Peer
  -- run buyer Peer
  void $ runPeerWithDriver buyerDriver buyerPeer
