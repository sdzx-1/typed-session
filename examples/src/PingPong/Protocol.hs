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

module PingPong.Protocol where

import PingPong.Type
import TypedSession.Core

[pingpongProtocl|

  Label 0
    Branch Client {
      BranchSt CheckVal
          Msg "Check" ["Int"] Client Counter
          Msg "CheckResult" ["Bool"] Counter Client
          Goto 0
      BranchSt STrue
          Msg "Ping" [] Client Server
          Msg "Pong" [] Server Client
          Msg "AddOne" [] Client Counter
          Goto 0
      BranchSt SFalse
          Msg "Stop" [] Client Server
          Msg "CStop" [] Client Counter
          Terminal
    }

|]

instance Show (AnyMsg PingPongRole PingPong) where
  show (AnyMsg msg) = case msg of
    Ping -> "Ping"
    Pong -> "Pong"
    Stop -> "Stop"
    AddOne -> "AddOne"
    CStop -> "CStop"
    Check i -> "CheckValue " ++ show i
    CheckResult b -> "CheckResult " ++ show b
