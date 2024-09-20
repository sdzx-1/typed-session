{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE QualifiedDo #-}
{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}

module Ring.Peer where

import Control.Concurrent.Class.MonadSTM (newTVarIO)
import Control.Monad (forM, void)
import Control.Monad.Class.MonadFork (forkIO)
import Data.IFunctor (At, returnAt)
import qualified Data.IFunctor as I
import qualified Data.IntMap as IntMap
import GHC.Base (Any)
import Ring.Protocol
import Ring.Type
import TypedSession.Core
import TypedSession.Driver (SomeRole (..), localDriverSimple, runPeerWithDriver)

choice :: Int -> ChoiceNAFun IO
choice i =
  if i >= 3
    then liftConstructor BranchSt_Stop
    else liftConstructor BranchSt_Continue

aPeer :: Int -> Peer RingRole Ring A IO (At () (Done A)) (AStartSt)
aPeer i = I.do
  choice i I.>>= \case
    BranchSt_Stop -> I.do
      yield BStop
      DStopA <- await
      returnAt ()
    BranchSt_Continue -> I.do
      yield AB
      DA <- await
      aPeer (i + 1)

bPeer :: Peer RingRole Ring B IO (At () (Done B)) (BStartSt Any)
bPeer =
  await I.>>= \case
    BStop -> I.do
      yield CStop
    AB -> I.do
      yield BC
      bPeer

cPeer :: Peer RingRole Ring C IO (At () (Done C)) (CStartSt Any)
cPeer =
  await I.>>= \case
    CStop -> I.do
      yield DStop
    BC -> I.do
      yield CD
      cPeer

dPeer :: Peer RingRole Ring D IO (At () (Done D)) (DStartSt Any)
dPeer =
  await I.>>= \case
    DStop -> I.do
      yield DStopA
    CD -> I.do
      yield DA
      dPeer

main :: IO ()
main = do
  let rg = [A .. D]
  vs <- forM rg $ \r -> do
    tvar <- newTVarIO IntMap.empty
    pure (fromEnum r, tvar)
  let allMap = IntMap.fromList vs
      driver someRole = localDriverSimple (\v -> putStrLn (show v)) allMap someRole id
  forkIO $ void $ runPeerWithDriver (driver $ SomeRole SB) bPeer
  forkIO $ void $ runPeerWithDriver (driver $ SomeRole SC) cPeer
  forkIO $ void $ runPeerWithDriver (driver $ SomeRole SD) dPeer
  void $ runPeerWithDriver (driver $ SomeRole SA) (aPeer 0)