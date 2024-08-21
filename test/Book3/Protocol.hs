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

module Book3.Protocol where

import Book3.Type

import Data.IFunctor (Sing, SingI (sing))
import GHC.Exts (Int (..), dataToTag#)
import TypedSession.Codec
import TypedSession.Core

[bookProtocol|
  Label 0
    Msg "Title" ["String"] Buyer Seller
    Branch Seller
        BranchSt NotFound 
          Msg "NoBook" [] Seller Buyer
               Msg "SellerNoBook" [] Buyer Buyer2
               Terminal
        BranchSt Found 
          Msg "Price" ["Int"] Seller Buyer
          Branch Buyer
            BranchSt One 
              Msg "OneAccept" [] Buyer Seller
              Msg "OneDate" ["Int"] Seller Buyer
              Msg "OneSuccess" ["Int"] Buyer Buyer2
              Terminal
            BranchSt Two 
              Msg "PriceToBuyer2" ["Int"] Buyer Buyer2
              Branch Buyer2
                BranchSt NotSupport 
                  Msg "NotSupport1" [] Buyer2 Buyer
                  Msg "TwoNotBuy" [] Buyer Seller
                  Terminal
                BranchSt Support 
                  Msg "SupportVal" ["Int"] Buyer2 Buyer
                  Branch Buyer
                    BranchSt Enough 
                      Msg "TwoAccept" [] Buyer Seller
                      Msg "TwoDate" ["Int"] Seller Buyer
                      Msg "TwoSuccess" ["Int"] Buyer Buyer2
                      Terminal
                    BranchSt NotEnough 
                      Msg "TwoNotBuy1" [] Buyer Seller
                      Msg "TwoFailed" [] Buyer Buyer2
                      Terminal

|]

{-
-----------------------------------------------Buyer-------------------------Seller------------------------Buyer2
LABEL 0                                          S0                            S0                           S1 s
  Title                                         S0->                          ->S0                          S1 s
  [Branch Seller]                               S2 s                           S3                           S1 s
    * BranchSt NotFound
    NoBook                                     S2 s<-                   <-{S2 NotFound}                     S1 s
    SellerNoBook                           S1 NotFound->                      End                          ->S1 s
    ~ Terminal                                  End                           End                           End
    * BranchSt Found
    Price                                      S2 s<-                     <-{S2 Found}                      S1 s
    [Branch Buyer]                               S7                           S8 s                          S1 s
      * BranchSt One
      OneAccept                              {S8 One}->                      ->S8 s                         S1 s
      OneDate                                  S12<-                         <-S12                          S1 s
      OneSuccess                              S1 One->                        End                          ->S1 s
      ~ Terminal                                End                           End                           End
      * BranchSt Two
      PriceToBuyer2                          {S1 Two}->                       S8 s                         ->S1 s
      [Branch Buyer2]                          S13 s                          S8 s                          S14
        * BranchSt NotSupport
        NotSupport1                           S13 s<-                         S8 s                   <-{S13 NotSupport}
        TwoNotBuy                         S8 NotSupport->                    ->S8 s                         End
        ~ Terminal                              End                           End                           End
        * BranchSt Support
        SupportVal                            S13 s<-                         S8 s                    <-{S13 Support}
        [Branch Buyer]                          S18                           S8 s                         S19 s
          * BranchSt Enough
          TwoAccept                        {S8 Enough}->                     ->S8 s                        S19 s
          TwoDate                              S23<-                         <-S23                         S19 s
          TwoSuccess                        S19 Enough->                      End                         ->S19 s
          ~ Terminal                            End                           End                           End
          * BranchSt NotEnough
          TwoNotBuy1                      {S8 NotEnough}->                   ->S8 s                        S19 s
          TwoFailed                       S19 NotEnough->                     End                         ->S19 s
          ~ Terminal                            End                           End                           End

-}

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