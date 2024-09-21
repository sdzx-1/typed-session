{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -ddump-splices #-}

module KV.Protocol where

import KV.Type
import TypedSession.Core

[kvProtocol|

Label 0
Branch Client ChoiceNextAction {
  BranchSt Stop [] 
    Msg PStop [] Client Primary 
    Msg BStop [] Client Backup
    Terminal
  BranchSt Put [String, String]
    Msg PutKV [String, String] Client Primary
    Msg BackupKV [String, String] Primary Backup
    Goto 0
  BranchSt Get [String]
    Msg GetKey [String] Client Primary
    Msg GetKeyResult [Maybe String] Primary Client
    Goto 0
}

|]

instance Show (AnyMsg KVRole KV) where
  show (AnyMsg msg) = case msg of
    PStop -> "PStop"
    BStop -> "BStop"
    PutKV k v -> "PutKV: " <> k <> "," <> v
    BackupKV k v -> "BackupKV: " <> k <> "," <> v
    GetKey k -> "GetKey: " <> k
    GetKeyResult r -> "GetKeyResult: " <> show r

data Request = RExit | RPut String String | RGet String deriving (Show, Read)

readRequest :: IO Request
readRequest = do
  putStrLn "Command?"
  line <- getLine
  case parseRequest line of
    Just t -> return t
    Nothing -> putStrLn "Invalid command" >> readRequest
 where
  parseRequest :: String -> Maybe Request
  parseRequest s =
    let l = words s
     in case l of
          ["GET", k] -> Just (RGet k)
          ["PUT", k, v] -> Just (RPut k v)
          "q" : _ -> Just RExit
          _ -> Nothing
