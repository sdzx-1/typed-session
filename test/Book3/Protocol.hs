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
{-# OPTIONS_GHC -Wno-unused-do-bind #-}

module Book3.Protocol where

import Book3.Type

import TypedSession.Codec
import TypedSession.Core


[bookProtocol|
  Label 0
    Msg Title [String] Buyer Seller
    Branch Seller FindBookResult {
        BranchSt NotFound []
          Msg NoBook [] Seller Buyer
          Msg SellerNoBook [] Buyer Buyer2
          Terminal
        BranchSt Found []
          Msg Price [Int] Seller Buyer
          Branch Buyer OneOrTwo {
            BranchSt One []
              Msg OneAccept [] Buyer Seller
              Msg OneDate [Int] Seller Buyer
              Msg OneSuccess [Int] Buyer Buyer2
              Terminal
            BranchSt Two []
              Msg PriceToBuyer2 [Int] Buyer Buyer2
              Branch Buyer2 SupportOrNotSupport {
                BranchSt NotSupport []
                  Msg NotSupport1 [] Buyer2 Buyer
                  Msg TwoNotBuy [] Buyer Seller
                  Terminal
                BranchSt Support []
                  Msg SupportVal [Int] Buyer2 Buyer
                  Branch Buyer EnoughOrNotEnough {
                    BranchSt Enough []
                      Msg TwoAccept [] Buyer Seller
                      Msg TwoDate [Int] Seller Buyer
                      Msg TwoSuccess [Int] Buyer Buyer2
                      Terminal
                    BranchSt NotEnough []
                      Msg TwoNotBuy1 [] Buyer Seller
                      Msg TwoFailed [] Buyer Buyer2
                      Terminal 
                }
              }
          }
        }

|]

encodeMsg :: Encode BookRole Book (AnyMsg BookRole Book)
encodeMsg = Encode $ \x -> case x of
  Title{} -> AnyMsg x
  NoBook{} -> AnyMsg x
  SellerNoBook{} -> AnyMsg x
  Price{} -> AnyMsg x
  OneAccept{} -> AnyMsg x
  OneDate{} -> AnyMsg x
  OneSuccess{} -> AnyMsg x
  PriceToBuyer2{} -> AnyMsg x
  NotSupport1{} -> AnyMsg x
  TwoNotBuy{} -> AnyMsg x
  SupportVal{} -> AnyMsg x
  TwoAccept{} -> AnyMsg x
  TwoDate{} -> AnyMsg x
  TwoSuccess{} -> AnyMsg x
  TwoNotBuy1{} -> AnyMsg x
  TwoFailed{} -> AnyMsg x

decodeMsg
  :: DecodeStep
      (AnyMsg BookRole Book)
      CodecFailure
      (AnyMsg BookRole Book)
decodeMsg =
  DecodePartial $ \case
    Nothing -> DecodeFail (CodecFailure "expected more data")
    Just anyMsg -> DecodeDone anyMsg Nothing

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
