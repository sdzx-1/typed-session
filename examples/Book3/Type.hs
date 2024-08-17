{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE QualifiedDo #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module Type where

import Data.IFunctor (Sing, SingI)
import qualified Data.IFunctor as I
import Data.Kind
import GHC.Exts (dataToTag#)
import GHC.Int (Int (I#))
import TypedSession.Core

{-
-----------------------------------------------Buyer-------------------------Seller------------------------Buyer2
LABEL 0                                          S0                            S0                           S1 s
  Title                                         S0->                          ->S0                          S1 s
  [Branch] Seller                               S2 s                          S2 s                          S1 s
    * BranchSt NotFound
    NoBook                                     S2 s<-                    <-S2 NotFound                      S1 s
    SellerNoBook                           S1 NotFound->                       S0                          ->S1 s
    ^ Goto 0                                     S0                            S0                           S1 s
    * BranchSt Found
    Price                                      S2 s<-                      <-S2 Found                       S1 s
    [Branch] Buyer                              S1 s                          S3 s                          S1 s
      * BranchSt One
      OneAfford                               S1 One->                        S3 s                         ->S1 s
      OneAccept                               S3 One->                       ->S3 s                          S4
      OneDate                                   S5<-                          <-S5                           S4
      OneSuccess                                S4->                           S0                           ->S4
      ^ Goto 0                                   S0                            S0                           S1 s
      * BranchSt Two
      PriceToBuyer2                           S1 Two->                        S3 s                         ->S1 s
      [Branch] Buyer2                           S6 s                          S3 s                          S6 s
        * BranchSt NotSupport
        NotSupport1                            S6 s<-                         S3 s                    <-S6 NotSupport
        TwoNotBuy                         S3 NotSupport->                    ->S3 s                         S1 s
        ^ Goto 0                                 S0                            S0                           S1 s
        * BranchSt Support
        SupportVal                             S6 s<-                         S3 s                      <-S6 Support
        [Branch] Buyer                          S3 s                          S3 s                          S7 s
          * BranchSt Enough
          TwoAccept                         S3 Enough->                      ->S3 s                         S7 s
          TwoDate                               S8<-                          <-S8                          S7 s
          TwoSuccess                        S7 Enough->                        S0                          ->S7 s
          ^ Goto 0                               S0                            S0                           S1 s
          * BranchSt NotEnough
          TwoNotBuy1                       S3 NotEnough->                    ->S3 s                         S7 s
          TwoFailed                        S7 NotEnough->                     End                          ->S7 s
          ~ Terminal                            End                           End                           End

-}

data Role
  = Buyer
  | Seller
  | Buyer2
  deriving (Show, Eq, Ord, Enum, Bounded)

data BookBranchSt
  = NotFound
  | Found
  | One
  | Two
  | Support
  | NotSupport
  | Enough
  | NotEnough
  deriving (Show)

data SRole :: Role -> Type where
  SBuyer :: SRole Buyer
  SSeller :: SRole Seller
  SBuyer2 :: SRole Buyer2
type instance Sing = SRole
instance SingI Buyer where
  sing = SBuyer
instance SingI Seller where
  sing = SSeller
instance SingI Buyer2 where
  sing = SBuyer2
instance SingToInt Role where
  singToInt x = I# (dataToTag# x)
data BookSt
  = End
  | S0
  | S1 BookBranchSt
  | S2 BookBranchSt
  | S3 BookBranchSt
  | S4
  | S5
  | S6 BookBranchSt
  | S7 BookBranchSt
  | S8
data SBookSt :: BookSt -> Type where
  SEnd :: SBookSt End
  SS0 :: SBookSt S0
  SS1 :: SBookSt (S1 s)
  SS2 :: SBookSt (S2 s)
  SS3 :: SBookSt (S3 s)
  SS4 :: SBookSt S4
  SS5 :: SBookSt S5
  SS6 :: SBookSt (S6 s)
  SS7 :: SBookSt (S7 s)
  SS8 :: SBookSt S8
type instance Sing = SBookSt
instance SingI End where
  sing = SEnd
instance SingI S0 where
  sing = SS0
instance SingI (S1 s) where
  sing = SS1
instance SingI (S2 s) where
  sing = SS2
instance SingI (S3 s) where
  sing = SS3
instance SingI S4 where
  sing = SS4
instance SingI S5 where
  sing = SS5
instance SingI (S6 s) where
  sing = SS6
instance SingI (S7 s) where
  sing = SS7
instance SingI S8 where
  sing = SS8
instance SingToInt BookSt where
  singToInt x = I# (dataToTag# x)
instance Protocol Role BookSt where
  type Done Buyer = End
  type Done Seller = End
  type Done Buyer2 = End
  data Msg Role BookSt from send recv where
    Title :: String -> Msg Role BookSt (S0) '(Buyer, S2 s) '(Seller, S2 s)
    NoBook :: Msg Role BookSt (S2 NotFound) '(Seller, S0) '(Buyer, S1 NotFound)
    SellerNoBook :: Msg Role BookSt (S1 NotFound) '(Buyer, S0) '(Buyer2, S1 s)
    Price :: Int -> Msg Role BookSt (S2 Found) '(Seller, S3 s) '(Buyer, S1 s)
    OneAfford :: Msg Role BookSt (S1 One) '(Buyer, S3 One) '(Buyer2, S4)
    OneAccept :: Msg Role BookSt (S3 One) '(Buyer, S5) '(Seller, S5)
    OneDate :: Int -> Msg Role BookSt (S5) '(Seller, S0) '(Buyer, S4)
    OneSuccess :: Int -> Msg Role BookSt (S4) '(Buyer, S0) '(Buyer2, S1 s)
    PriceToBuyer2 :: Int -> Msg Role BookSt (S1 Two) '(Buyer, S6 s) '(Buyer2, S6 s)
    NotSupport1 :: Msg Role BookSt (S6 NotSupport) '(Buyer2, S1 s) '(Buyer, S3 NotSupport)
    TwoNotBuy :: Msg Role BookSt (S3 NotSupport) '(Buyer, S0) '(Seller, S0)
    SupportVal :: Int -> Msg Role BookSt (S6 Support) '(Buyer2, S7 s) '(Buyer, S3 s)
    TwoAccept :: Msg Role BookSt (S3 Enough) '(Buyer, S8) '(Seller, S8)
    TwoDate :: Int -> Msg Role BookSt (S8) '(Seller, S0) '(Buyer, S7 Enough)
    TwoSuccess :: Int -> Msg Role BookSt (S7 Enough) '(Buyer, S0) '(Buyer2, S1 s)
    TwoNotBuy1 :: Msg Role BookSt (S3 NotEnough) '(Buyer, S7 NotEnough) '(Seller, End)
    TwoFailed :: Msg Role BookSt (S7 NotEnough) '(Buyer, End) '(Buyer2, End)

instance Show (AnyMsg Role BookSt) where
  show (AnyMsg msg) = case msg of
    Title st -> "Title " <> show st
    NoBook -> "NoBook"
    SellerNoBook -> "SellerNoBook"
    Price i -> "Price " <> show i
    OneAfford -> "OneAfford"
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