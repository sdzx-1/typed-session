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
    Branch Seller {
        BranchSt NotFound
          Msg "NoBook" [] Seller Buyer
               Msg "SellerNoBook" [] Buyer Buyer2
               Goto 0
        BranchSt Found 
          Msg "Price" ["Int"] Seller Buyer
          Branch Buyer {
            BranchSt One 
              Msg "OneAccept" [] Buyer Seller
              Msg "OneDate" ["Int"] Seller Buyer
              Msg "OneSuccess" ["Int"] Buyer Buyer2
              Goto 0
            BranchSt Two 
              Msg "PriceToBuyer2" ["Int"] Buyer Buyer2
              Branch Buyer2 {
                BranchSt NotSupport 
                  Msg "NotSupport1" [] Buyer2 Buyer
                  Msg "TwoNotBuy" [] Buyer Seller
                  Goto 0
                BranchSt Support 
                  Msg "SupportVal" ["Int"] Buyer2 Buyer
                  Branch Buyer {
                    BranchSt Enough 
                      Msg "TwoAccept" [] Buyer Seller
                      Msg "TwoDate" ["Int"] Seller Buyer
                      Msg "TwoSuccess" ["Int"] Buyer Buyer2
                      Goto 0
                    BranchSt NotEnough 
                      Msg "TwoNotBuy1" [] Buyer Seller
                      Msg "TwoFailed" [] Buyer Buyer2
                      Terminal
                }
              }
          }
        }

|]

{-
-----------------------------------------------Buyer-------------------------Seller------------------------Buyer2
LABEL 0                                          S0                            S0                           S1 s
  Title                                         S0->                          ->S0                          S1 s
  [Branch Seller]                               S2 s                           S3                           S1 s
    NoBook                                     S2 s<-                   <-{S2 NotFound}                     S1 s
      SellerNoBook                         S1 NotFound->                      End                          ->S1 s
      Terminal                                  End                           End                           End
    Price                                      S2 s<-                     <-{S2 Found}                      S1 s
      [Branch Buyer]                             S4                           S5 s                          S1 s
        OneAccept                            {S5 One}->                      ->S5 s                         S1 s
          OneDate                              S10<-                         <-S10                          S1 s
          OneSuccess                          S1 One->                        End                          ->S1 s
          Terminal                              End                           End                           End
        PriceToBuyer2                        {S1 Two}->                       S5 s                         ->S1 s
          [Branch Buyer2]                       S6 s                          S5 s                           S7
            NotSupport1                        S6 s<-                         S5 s                   <-{S6 NotSupport}
              TwoNotBuy                   S5 NotSupport->                    ->S5 s                         End
              Terminal                          End                           End                           End
            SupportVal                         S6 s<-                         S5 s                     <-{S6 Support}
              [Branch Buyer]                     S8                           S5 s                          S9 s
                TwoAccept                  {S5 Enough}->                     ->S5 s                         S9 s
                  TwoDate                      S11<-                         <-S11                          S9 s
                  TwoSuccess                S9 Enough->                       End                          ->S9 s
                  Terminal                      End                           End                           End
                TwoNotBuy1                {S5 NotEnough}->                   ->S5 s                         S9 s
                  TwoFailed                S9 NotEnough->                     End                          ->S9 s
                  Terminal                      End                           End                           End

-}

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