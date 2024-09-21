{-# LANGUAGE TemplateHaskellQuotes #-}
{-# LANGUAGE TypeApplications #-}

module KV.Type where

-- origin example: https://github.com/gshen42/HasChor/blob/main/examples/kvs-2-primary-backup/Main.hs
import TypedSession.TH

data KVRole = Client | Primary | Backup
  deriving (Eq, Ord, Show, Enum, Bounded)

data KVBranchSt = Stop | Put | Get
  deriving (Eq, Ord, Show, Enum, Bounded)

kvProtocol = protocol @KVRole @KVBranchSt "KV" ''KVRole ''KVBranchSt