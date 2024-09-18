{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Ring.Protocol where

import Ring.Type
import TypedSession.Core

[ringPrtocol|

Label 0
Branch A ChoiceNA {
  BranchSt Stop [] 
    Msg BStop [] A B
    Msg CStop [] B C
    Msg DStop [] C D
    Msg DStopA [] D A
    Terminal
  BranchSt Continue []
    Msg AB [] A B 
    Msg BC [] B C
    Msg CD [] C D
    Msg DA [] D A
    Goto 0
}

|]

instance Show (AnyMsg RingRole Ring) where
  show (AnyMsg msg) = case msg of
    BStop -> "BStop"
    CStop -> "CStop"
    DStop -> "DStop"
    DStopA -> "DStopA"
    AB -> "AB"
    BC -> "BC"
    CD -> "CD"
    DA -> "DA"