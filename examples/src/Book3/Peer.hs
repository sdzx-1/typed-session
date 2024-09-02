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
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wall #-}

module Book3.Peer where

import Book3.Protocol
import Book3.Type
import Control.Algebra ((:+:))
import Control.Effect.Random (Random, uniform)
import Control.Effect.State
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
  -> EnoughtOrNotEnoughFun m
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
    FinishBuyer -> I.do
      yield FinishBuyer2
      returnAt Nothing
    NoBook -> I.do
      yield SellerNoBook
      buyerPeer
    (Price i) -> I.do
      choiceOT i I.>>= \case
        BranchSt_One -> I.do
          yield OneAccept
          (OneDate d) <- await
          yield (OneSuccess d)
          buyerPeer
        BranchSt_Two -> I.do
          yield (PriceToBuyer2 (i `div` 2))
          await I.>>= \case
            NotSupport1 -> I.do
              yield TwoNotBuy
              buyerPeer
            (SupportVal h) -> I.do
              checkPrice 10 h I.>>= \case
                BranchSt_Enough -> I.do
                  yield TwoAccept
                  (TwoDate d) <- await
                  yield (TwoSuccess d)
                  buyerPeer
                BranchSt_NotEnough -> I.do
                  yield TwoNotBuy1
                  yield TwoFailed
                  buyerPeer

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
    FinishBuyer2 -> returnAt Nothing
    SellerNoBook -> buyer2Peer
    (OneSuccess d) -> buyer2Peer
    (PriceToBuyer2 i) -> I.do
      choiceB i I.>>= \case
        BranchSt_NotSupport -> I.do
          yield NotSupport1
          buyer2Peer
        BranchSt_Support -> I.do
          yield (SupportVal (i `div` 2))
          await I.>>= \case
            (TwoSuccess d) -> buyer2Peer
            TwoFailed -> buyer2Peer

findBook
  :: (Has (Random :+: State Int) sig m)
  => String
  -> ChoiceActionFun m
findBook _st = I.do
  At i <- liftm $ get @Int
  if i > 30
    then liftConstructor BranchSt_Finish
    else I.do
      At b <- liftm $ uniform @Bool
      if b
        then liftConstructor BranchSt_Found
        else liftConstructor BranchSt_NotFound

sellerPeer
  :: (Has (Random :+: State Int) sig m)
  => Peer BookRole Book Seller m (At () (Done Seller)) SellerStartSt
sellerPeer = I.do
  liftm $ modify @Int (+ 1)
  (Title st) <- await
  findBook st I.>>= \case
    BranchSt_Finish -> I.do
      yield FinishBuyer
    BranchSt_NotFound -> I.do
      yield NoBook
      sellerPeer
    BranchSt_Found -> I.do
      yield (Price 30)
      await I.>>= \case
        OneAccept -> I.do
          yield (OneDate 100)
          sellerPeer
        TwoNotBuy -> sellerPeer
        TwoAccept -> I.do
          yield (TwoDate 100)
          sellerPeer
        TwoNotBuy1 -> sellerPeer
