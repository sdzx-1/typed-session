{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE QualifiedDo #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Book3.Peer where

import Book3.Protocol
import Book3.Type
import Control.Algebra (Has)
import Control.Effect.Random (Random, uniform)
import Data.IFunctor (At (..), returnAt)
import qualified Data.IFunctor as I
import TypedSession.Core

budget :: Int
budget = 16

type Date = Int

checkPrice
  :: (Has Random sig m)
  => Int
  -> Int
  -> EnoughOrNotEnoughFun m
checkPrice _i _h = I.do
  At b <- liftm $ uniform @Bool
  if b
    then liftConstructor BranchSt_Enough
    else liftConstructor BranchSt_NotEnough

choiceOT
  :: (Has Random sig m)
  => Int
  -> OneOrTwoFun m
choiceOT _i = I.do
  At b <- liftm $ uniform @Bool
  if b
    then liftConstructor BranchSt_One
    else liftConstructor BranchSt_Two

buyerPeer
  :: (Has Random sig m)
  => Peer BookRole Book Buyer m (At (Maybe Date) (Done Buyer)) BuyerStartSt
buyerPeer = I.do
  yield (Title "haskell book")
  await I.>>= \case
    NoBook -> I.do
      yield SellerNoBook
      returnAt Nothing
    Price i -> I.do
      choiceOT i I.>>= \case
        BranchSt_One -> I.do
          yield OneAccept
          OneDate d <- await
          yield (OneSuccess d)
          returnAt $ Just d
        BranchSt_Two -> I.do
          yield (PriceToBuyer2 (i `div` 2))
          await I.>>= \case
            NotSupport1 -> I.do
              yield TwoNotBuy
              returnAt Nothing
            SupportVal h -> I.do
              checkPrice 10 h I.>>= \case
                BranchSt_Enough -> I.do
                  yield TwoAccept
                  TwoDate d <- await
                  yield (TwoSuccess d)
                  returnAt (Just d)
                BranchSt_NotEnough -> I.do
                  yield TwoNotBuy1
                  yield TwoFailed
                  returnAt Nothing

choiceB
  :: (Has Random sig m)
  => Int
  -> SupportOrNotSupportFun m
choiceB _i = I.do
  At b <- liftm $ uniform @Bool
  if b
    then liftConstructor BranchSt_Support
    else liftConstructor BranchSt_NotSupport

buyer2Peer
  :: (Has Random sig m)
  => Peer BookRole Book Buyer2 m (At (Maybe Date) (Done Buyer2)) (Buyer2StartSt s)
buyer2Peer = I.do
  await I.>>= \case
    SellerNoBook -> returnAt Nothing
    (OneSuccess d) -> returnAt (Just d)
    (PriceToBuyer2 i) -> I.do
      choiceB i I.>>= \case
        BranchSt_NotSupport -> I.do
          yield NotSupport1
          returnAt Nothing
        BranchSt_Support -> I.do
          yield (SupportVal (i `div` 2))
          await I.>>= \case
            TwoSuccess d -> returnAt $ Just d
            TwoFailed -> returnAt Nothing

findBook
  :: (Has Random sig m)
  => String
  -> FindBookResultFun m
findBook _st = I.do
  At b <- liftm $ uniform @Bool
  if b
    then liftConstructor BranchSt_Found
    else liftConstructor BranchSt_NotFound

sellerPeer
  :: (Has Random sig m)
  => Peer BookRole Book Seller m (At () (Done Seller)) SellerStartSt
sellerPeer = I.do
  (Title st) <- await
  findBook st I.>>= \case
    BranchSt_NotFound -> yield NoBook
    BranchSt_Found -> I.do
      yield (Price 30)
      await I.>>= \case
        OneAccept -> yield (OneDate 100)
        TwoNotBuy -> returnAt ()
        TwoAccept -> yield (TwoDate 100)
        TwoNotBuy1 -> returnAt ()
