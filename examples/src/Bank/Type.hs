{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE QualifiedDo #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}

module Bank.Type where

import Language.Haskell.TH.Quote
import TypedSession.TH
import Data.List.Split (splitOn)
import Data.Maybe (mapMaybe)
import Text.Read (readMaybe)

{-
bank-2pc

https://github.com/gshen42/HasChor/tree/main/examples/bank-2pc
-}

data BankRole = Client | Coordinator | Alice | Bob
  deriving (Show, Eq, Ord, Enum, Bounded)

data BankBranchSt = Continue | Finish | CTrue | CFalse
  deriving (Show, Read, Eq, Ord, Enum, Bounded)

bankProtocol :: QuasiQuoter
bankProtocol = protocol @BankRole @BankBranchSt "Bank" ''BankRole ''BankBranchSt

type Action = (String, Int)

type Transaction = [Action]

validate :: String -> Int -> Transaction -> (Bool, Int)
validate name balance tx =
  foldl (\(valid, i) (_, amount) -> (let next = i + amount in (valid && next >= 0, next))) (True, balance) actions
 where
  actions = filter (\(n, _) -> n == name) tx

parse :: String -> Transaction
parse s = tx
 where
  t = splitOn ";" s
  f :: String -> Maybe Action
  f l = do
    [target, amountStr] <- return $ words l
    amount <- readMaybe amountStr :: Maybe Int
    target' <- if target == "alice" || target == "bob" then Just target else Nothing
    return (target', amount)
  tx = mapMaybe f t