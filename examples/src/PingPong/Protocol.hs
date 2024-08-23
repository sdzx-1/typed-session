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

module PingPong.Protocol where

import qualified Data.IFunctor
import GHC.Base (Int (I#), dataToTag#)
import PingPong.Type
import TypedSession.Core

[pingpongProtocl|

  Label 0
    Branch Client {
      BranchSt STrue
          Msg "AddOne" [] Client Counter
          Msg "Recved" [] Counter Client
          Msg "Ping" [] Client Server
          Msg "Pong" [] Server Client
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
    Recved -> "Recved"
