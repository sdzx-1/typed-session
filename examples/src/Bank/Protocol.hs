{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -ddump-splices #-}

module Bank.Protocol where

import Bank.Type
import TypedSession.Core

[bankProtocol|
Label 0
Branch Client ChoiceNextAction {
  BranchSt Finish []
    Msg CStop [] Client Coordinator
    Msg BStop [] Coordinator Bob
    Msg AStop [] Coordinator Alice
    Terminal
  BranchSt Continue [Transaction]
    Msg Command [Transaction] Client Coordinator
    Msg Transaction1 [Transaction] Coordinator Alice
    Msg Transaction2 [Transaction] Coordinator Bob
    Msg AliceValidResult [Bool] Alice Coordinator
    Msg BobValidResult [Bool] Bob Coordinator
    Branch Coordinator ValidResult {
      BranchSt CFalse []
        Msg ValidFailed [] Coordinator Client
        Msg ValidFailedA [] Coordinator Alice
        Msg ValidFailedB [] Coordinator Bob
        Goto 0
      BranchSt CTrue []
        Msg ValidSuccessed [] Coordinator Client
        Msg ValidSuccessedA [] Coordinator Alice
        Msg ValidSuccessedB [] Coordinator Bob
        Goto 0

    }
}
|]

instance Show (AnyMsg BankRole Bank) where
  show (AnyMsg msg) = case msg of
    CStop -> "CStop"
    BStop -> "BStop"
    AStop -> "AStop"
    Command tr -> "Command " ++ show tr
    Transaction1 tr -> "Transaction1 " ++ show tr
    Transaction2 tr -> "Transaction2 " ++ show tr
    AliceValidResult b -> "AliceValidResult " ++ show b
    BobValidResult b -> "BobValidResult " ++ show b
    ValidFailed -> "ValidFailed"
    ValidFailedA -> "ValidFailedA"
    ValidFailedB -> "ValidFailedB"
    ValidSuccessed -> "ValidSuccessed"
    ValidSuccessedA -> "ValidSuccessedA"
    ValidSuccessedB -> "ValidSuccessedB"
