{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -ddump-splices #-}

module Book3.Protocol where

import Book3.Type

import Data.IFunctor (Sing, SingI (sing))
import GHC.Exts (Int (..), dataToTag#)
import TypedSession.Codec
import TypedSession.Core

[bookProtocol|
  Label 0
    Msg Title [String] Buyer Seller
    Branch Seller ChoiceAction{
        BranchSt Finish []
          Msg FinishBuyer [] Seller Buyer
          Msg FinishBuyer2 [] Buyer Buyer2
          Terminal
        BranchSt NotFound []
          Msg NoBook [] Seller Buyer
               Msg SellerNoBook [] Buyer Buyer2
               Goto 0
        BranchSt Found []
          Msg Price [Int] Seller Buyer
          Branch Buyer OneOrTwo {
            BranchSt One []
              Msg OneAccept [] Buyer Seller
              Msg OneDate [Int] Seller Buyer
              Msg OneSuccess [Int] Buyer Buyer2
              Goto 0
            BranchSt Two []
              Msg PriceToBuyer2 [Int] Buyer Buyer2
              Branch Buyer2 SupportOrNotSupport {
                BranchSt NotSupport []
                  Msg NotSupport1 [] Buyer2 Buyer
                  Msg TwoNotBuy [] Buyer Seller
                  Goto 0
                BranchSt Support []
                  Msg SupportVal [Int] Buyer2 Buyer
                  Branch Buyer EnoughtOrNotEnough {
                    BranchSt Enough []
                      Msg TwoAccept [] Buyer Seller
                      Msg TwoDate [Int] Seller Buyer
                      Msg TwoSuccess [Int] Buyer Buyer2
                      Goto 0
                    BranchSt NotEnough [] 
                      Msg TwoNotBuy1 [] Buyer Seller
                      Msg TwoFailed [] Buyer Buyer2
                      Goto 0
                }
              }
          }
        }

|]

instance Show (AnyMsg BookRole Book) where
  show (AnyMsg msg) = case msg of
    Title st -> "Title " <> show st
    NoBook -> "NoBook"
    SellerNoBook -> "SellerNoBook"
    Price i -> "Price " <> show i
    OneAccept -> "OneAccept"
    OneDate d -> "OneDate " <> show d
    OneSuccess d -> "OneSuccess" <> show d
    PriceToBuyer2 i -> "PriceToBuyer2 " <> show i
    NotSupport1 -> "NotSupport1"
    TwoNotBuy -> "TwoNotBuy"
    SupportVal v -> "SupportVal " <> show v
    TwoAccept -> "TwoAccept"
    TwoDate d -> "TwoDate " <> show d
    TwoSuccess d -> "TwoSuccess " <> show d
    TwoNotBuy1 -> "TwoNotBuy1"
    TwoFailed -> "TwoFailed"
    FinishBuyer -> "FinishBuyer"
    FinishBuyer2 -> "FinishBuyer2"