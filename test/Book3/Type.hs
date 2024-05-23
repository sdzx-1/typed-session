{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE QualifiedDo #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module Book3.Type where

import Data.IFunctor (Sing, SingI)
import qualified Data.IFunctor as I
import Data.Kind
import GHC.Exts (dataToTag#)
import GHC.Int (Int (I#))
import TypedProtocol.Codec
import TypedProtocol.Core

{-
---------------------------Buyer-------------------------Seller------------------------Buyer2---------------------------
                             S0                            S0                          (S1 s)
            Title            |            ----->           |
                           (S2 s)                        (S2 s)                        (S1 s)
    ---------------------------------------------------[NotFound]---------------------------------------------------
                           (S2 s)                   (S2 [NotFound])                    (S1 s)
            NoBook           |            <-----           |
                      (S1 [NotFound])                     End                          (S1 s)
         SellerNoBook        |                           ----->                          |
                            End                           End                           End
                                                        Terminal

    ----------------------------------------------------[Found]-----------------------------------------------------
                           (S2 s)                     (S2 [Found])                     (S1 s)
            Price            |            <-----           |
                           (S1 s)                        (S3 s)                        (S1 s)
        ----------------------------------------------[One,Found]-----------------------------------------------
                      (S1 [One,Found])                   (S3 s)                        (S1 s)
          OneAfford          |                           ----->                          |
                      (S3 [One,Found])                   (S3 s)                          S4
          OneAccept          |            ----->           |
                             S5                            S5                            S4
           OneDate           |            <-----           |
                             S4                           End                            S4
          OneSuccess         |                           ----->                          |
                            End                           End                           End
                                                        Terminal

        ----------------------------------------------[Two,Found]-----------------------------------------------
                      (S1 [Two,Found])                   (S3 s)                        (S1 s)
        PriceToBuyer2        |                           ----->                          |
                           (S6 s)                        (S3 s)                        (S6 s)
            -------------------------------------[NotSupport,Two,Found]-------------------------------------
                           (S6 s)                        (S3 s)             (S6 [NotSupport,Two,Found])
          NotSupport1        |                           <-----                          |
                (S3 [NotSupport,Two,Found])              (S3 s)                         End
          TwoNotBuy          |            ----->           |
                            End                           End                           End
                                                        Terminal

            --------------------------------------[Support,Two,Found]---------------------------------------
                           (S6 s)                        (S3 s)               (S6 [Support,Two,Found])
          SupportVal         |                           <-----                          |
                           (S3 s)                        (S3 s)                        (S7 s)
                -------------------------------[Enough,Support,Two,Found]-------------------------------
              (S3 [Enough,Support,Two,Found])            (S3 s)                        (S7 s)
          TwoAccept          |            ----->           |
                             S8                            S8                          (S7 s)
           TwoDate           |            <-----           |
              (S7 [Enough,Support,Two,Found])             End                          (S7 s)
          TwoSuccess         |                           ----->                          |
                            End                           End                           End
                                                        Terminal

                -----------------------------[NotEnough,Support,Two,Found]------------------------------
             (S3 [NotEnough,Support,Two,Found])          (S3 s)                        (S7 s)
          TwoNotBuy1         |            ----->           |
             (S7 [NotEnough,Support,Two,Found])           End                          (S7 s)
          TwoFailed          |                           ----->                          |
                            End                           End                           End
                                                        Terminal

-}

data Role = Buyer | Seller | Buyer2
  deriving (Show, Eq, Ord)

data SRole :: Role -> Type where
  SBuyer :: SRole Buyer
  SBuyer2 :: SRole Buyer2
  SSeller :: SRole Seller

type instance Sing = SRole

instance SingI Buyer where
  sing = SBuyer

instance SingI Buyer2 where
  sing = SBuyer2

instance SingI Seller where
  sing = SSeller

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

data BookSt
  = S0
  | S1 [BookBranchSt]
  | S2 [BookBranchSt]
  | S3 [BookBranchSt]
  | S4
  | S5
  | S6 [BookBranchSt]
  | S7 [BookBranchSt]
  | S8
  | End

data SBookSt :: BookSt -> Type where
  SS0 :: SBookSt S0
  SS1 :: SBookSt (S1 s)
  SS2 :: SBookSt (S2 s)
  SS3 :: SBookSt (S3 s)
  SS4 :: SBookSt S4
  SS5 :: SBookSt S5
  SS6 :: SBookSt (S6 s)
  SS7 :: SBookSt (S7 s)
  SS8 :: SBookSt S8
  SEnd :: SBookSt End

type instance Sing = SBookSt

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

instance SingI End where
  sing = SEnd

type Date = Int

instance SingToInt Role where
  singToInt x = I# (dataToTag# x)

instance SingToInt BookSt where
  singToInt x = I# (dataToTag# x)

instance Protocol Role BookSt where
  type Done Buyer = End
  type Done Seller = End
  type Done Buyer2 = End

  data Msg Role BookSt from send recv where
    Title :: String -> Msg Role BookSt S0 '(Buyer, S2 s) '(Seller, S2 s)
    NoBook :: Msg Role BookSt (S2 '[NotFound]) '(Seller, End) '(Buyer, S1 '[NotFound])
    SellerNoBook :: Msg Role BookSt (S1 '[NotFound]) '(Buyer, End) '(Buyer2, End)
    Price :: Int -> Msg Role BookSt (S2 '[Found]) '(Seller, S3 s) '(Buyer, S1 s)
    OneAfford :: Msg Role BookSt (S1 '[One, Found]) '(Buyer, S3 '[One, Found]) '(Buyer2, S4)
    OneAccept :: Msg Role BookSt (S3 '[One, Found]) '(Buyer, S5) '(Seller, S5)
    OneDate :: Date -> Msg Role BookSt S5 '(Seller, End) '(Buyer, S4)
    OneSuccess :: Date -> Msg Role BookSt S4 '(Buyer, End) '(Buyer2, End)
    PriceToBuyer2 :: Int -> Msg Role BookSt (S1 '[Two, Found]) '(Buyer, S6 s) '(Buyer2, S6 s)
    NotSupport1 :: Msg Role BookSt (S6 '[NotSupport, Two, Found]) '(Buyer2, End) '(Buyer, S3 '[NotSupport, Two, Found])
    TwoNotBuy :: Msg Role BookSt (S3 '[NotSupport, Two, Found]) '(Buyer, End) '(Seller, End)
    SupportVal :: Int -> Msg Role BookSt (S6 '[Support, Two, Found]) '(Buyer2, S7 s) '(Buyer, S3 s)
    TwoAccept :: Msg Role BookSt (S3 '[Enough, Support, Two, Found]) '(Buyer, S8) '(Seller, S8)
    TwoDate :: Date -> Msg Role BookSt S8 '(Seller, End) '(Buyer, S7 '[Enough, Support, Two, Found])
    TwoSuccess :: Date -> Msg Role BookSt (S7 '[Enough, Support, Two, Found]) '(Buyer, End) '(Buyer2, End)
    TwoNotBuy1 :: Msg Role BookSt (S3 '[NotEnough, Support, Two, Found]) '(Buyer, S7 '[NotEnough, Support, Two, Found]) '(Seller, End)
    TwoFailed :: Msg Role BookSt (S7 '[NotEnough, Support, Two, Found]) '(Buyer, End) '(Buyer2, End)

encodeMsg :: Encode Role BookSt (AnyMsg Role BookSt)
encodeMsg = Encode $ \x -> case x of
  Title{} -> AnyMsg x
  NoBook{} -> AnyMsg x
  SellerNoBook{} -> AnyMsg x
  Price{} -> AnyMsg x
  OneAfford{} -> AnyMsg x
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
      (AnyMsg Role BookSt)
      CodecFailure
      (AnyMsg Role BookSt)
decodeMsg =
  DecodePartial $ \case
    Nothing -> DecodeFail (CodecFailure "expected more data")
    Just anyMsg -> DecodeDone anyMsg Nothing

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