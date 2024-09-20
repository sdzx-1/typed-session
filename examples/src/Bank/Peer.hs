{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE QualifiedDo #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}

module Bank.Peer where

import Bank.Protocol
import Bank.Type

import Control.Concurrent.Class.MonadSTM
import Control.Monad
import Control.Monad.Class.MonadFork (forkIO)
import Data.IFunctor (At (..), returnAt)
import qualified Data.IFunctor as I
import qualified Data.IntMap as IntMap
import TypedSession.Codec
import TypedSession.Core
import TypedSession.Driver

choice :: ChoiceNextActionFun IO
choice = I.do
  At tr <- liftm $ do
    putStrLn "Input command:"
    getLine
  case tr of
    "q" -> liftConstructor BranchSt_Finish
    st -> liftConstructor (BranchSt_Continue (parse st))

clientPeer
  :: Peer BankRole Bank Client IO (At () (Done Client)) ClientStartSt
clientPeer = do
  choice I.>>= \case
    BranchSt_Finish -> I.do
      yield CStop
    BranchSt_Continue tr -> I.do
      yield (Command tr)
      await I.>>= \case
        ValidFailed -> I.do
          liftm $ putStrLn "valid failed"
          clientPeer
        ValidSuccessed -> I.do
          liftm $ putStrLn "valid successed"
          clientPeer

validC :: Bool -> Bool -> ValidResultFun IO
validC ar br =
  if ar && br
    then liftConstructor BranchSt_CTrue
    else liftConstructor BranchSt_CFalse

coordinatorPeer
  :: Peer BankRole Bank Coordinator IO (At () (Done Coordinator)) (CoordinatorStartSt s)
coordinatorPeer = do
  await I.>>= \case
    CStop -> I.do
      yield BStop
      yield AStop
    Command tr -> I.do
      yield (Transaction1 tr)
      yield (Transaction2 tr)
      AliceValidResult ar <- await
      BobValidResult br <- await
      validC ar br I.>>= \case
        BranchSt_CFalse -> I.do
          yield ValidFailed
          yield ValidFailedA
          yield ValidFailedB
          coordinatorPeer
        BranchSt_CTrue -> I.do
          yield ValidSuccessed
          yield ValidSuccessedA
          yield ValidSuccessedB
          coordinatorPeer

alicePeer :: Int -> Peer BankRole Bank Alice IO (At () (Done Alice)) (AliceStartSt s)
alicePeer val = I.do
  liftm $ putStrLn $ "alice's val: " ++ show val
  await I.>>= \case
    Transaction1 tr -> I.do
      let (valid, val') = validate "alice" val tr
      yield (AliceValidResult valid)
      await I.>>= \case
        ValidFailedA -> alicePeer val
        ValidSuccessedA -> alicePeer val'
    AStop -> returnAt ()

bobPeer :: Int -> Peer BankRole Bank Bob IO (At () (Done Bob)) (BobStartSt s)
bobPeer val = I.do
  liftm $ putStrLn $ "Bob's val: " ++ show val
  await I.>>= \case
    Transaction2 tr -> I.do
      let (valid, val') = validate "bob" val tr
      yield (BobValidResult valid)
      await I.>>= \case
        ValidFailedB -> bobPeer val
        ValidSuccessedB -> bobPeer val'
    BStop -> returnAt ()

main :: IO ()
main = do
  let rg = [Client .. Bob]
  vs <- forM rg $ \r -> do
    tvar <- newTVarIO IntMap.empty
    pure (fromEnum r, tvar)
  let allMap = IntMap.fromList vs
      driver r = localDriverSimple (\v -> putStrLn (show v)) allMap r id
  forkIO $ void $ runPeerWithDriver (driver $ SomeRole SCoordinator) coordinatorPeer
  forkIO $ void $ runPeerWithDriver (driver $ SomeRole SAlice) (alicePeer 0)
  forkIO $ void $ runPeerWithDriver (driver $ SomeRole SBob) (bobPeer 0)
  void $ runPeerWithDriver (driver $ SomeRole SClient) clientPeer