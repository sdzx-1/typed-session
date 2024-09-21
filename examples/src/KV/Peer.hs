{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE QualifiedDo #-}
{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}

module KV.Peer where

import Control.Concurrent.Class.MonadSTM (MonadSTM (..))
import Control.Monad (void)
import Control.Monad.Class.MonadFork (forkIO)
import Data.IFunctor (At (At), returnAt)
import qualified Data.IFunctor as I
import Data.IORef
import qualified Data.IntMap as IntMap
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Traversable (for)
import KV.Protocol
import KV.Type
import TypedSession.Core
import TypedSession.Driver (SomeRole (SomeRole), localDriverSimple, runPeerWithDriver)

choice :: ChoiceNextActionFun IO
choice = I.do
  At st <- liftm $ readRequest
  case st of
    RExit -> liftConstructor BranchSt_Stop
    RPut k v -> liftConstructor (BranchSt_Put k v)
    RGet k -> liftConstructor (BranchSt_Get k)

clientPeer :: Peer KVRole KV Client IO (At () (Done Client)) ClientStartSt
clientPeer = I.do
  choice I.>>= \case
    BranchSt_Stop -> I.do
      yield PStop
      yield BStop
    BranchSt_Put k v -> I.do
      yield (PutKV k v)
      clientPeer
    BranchSt_Get k -> I.do
      yield (GetKey k)
      GetKeyResult res <- await
      liftm $ putStrLn $ "Result: " <> show res
      clientPeer

primaryPeer
  :: IORef (Map String String)
  -> Peer KVRole KV Primary IO (At () (Done Primary)) (PrimaryStartSt s)
primaryPeer kvmapRef = I.do
  await I.>>= \case
    PStop -> returnAt ()
    PutKV k v -> I.do
      liftm $ modifyIORef' kvmapRef (Map.insert k v)
      yield (BackupKV k v)
      primaryPeer kvmapRef
    GetKey k -> I.do
      At v <- liftm $ (Map.lookup k) <$> readIORef kvmapRef
      yield (GetKeyResult v)
      primaryPeer kvmapRef

backupPeer :: IORef (Map String String) -> Peer KVRole KV Backup IO (At () (Done Backup)) (BackupStartSt s)
backupPeer kvmapRef = I.do
  await I.>>= \case
    BStop -> returnAt ()
    BackupKV k v -> I.do
      liftm $ do
        modifyIORef' kvmapRef (Map.insert k v)
        putStrLn $ "Backup: " <> k <> "," <> v
      backupPeer kvmapRef

main :: IO ()
main = do
  let rg = [Client .. Backup]
  vs <- for rg $ \r -> do
    tvar <- newTVarIO IntMap.empty
    pure (fromEnum r, tvar)
  let allMap = IntMap.fromList vs
      driver someRole = localDriverSimple (\v -> putStrLn (show v)) allMap someRole id
  kvmapP <- newIORef Map.empty
  kvmapB <- newIORef Map.empty
  forkIO $ void $ runPeerWithDriver (driver $ SomeRole SPrimary) (primaryPeer kvmapP)
  forkIO $ void $ runPeerWithDriver (driver $ SomeRole SBackup) (backupPeer kvmapB)
  void $ runPeerWithDriver (driver $ SomeRole SClient) clientPeer