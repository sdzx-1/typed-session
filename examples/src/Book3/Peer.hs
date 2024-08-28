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
import Control.Algebra (Has, (:+:))
import Control.Effect.Random (Random, uniform)
import Control.Effect.State
import Data.IFunctor (At (..), ireturn, returnAt)
import qualified Data.IFunctor as I
import Data.Kind
import TypedSession.Core

budget :: Int
budget = 16

type Date = Int

data CheckPriceResult :: Book -> Type where
  Yes :: CheckPriceResult (S5 Enough)
  No :: CheckPriceResult (S5 NotEnough)

checkPrice
  :: (Has Random sig m)
  => Int
  -> Int
  -> Peer BookRole Book Buyer m CheckPriceResult S8
checkPrice _i _h = I.do
  At b <- liftm $ uniform @Bool
  if b
    then LiftM $ pure (ireturn Yes)
    else LiftM $ pure (ireturn No)

data OT :: Book -> Type where
  OTOne :: OT (S5 One)
  OTTwo :: OT (S1 Two)

choiceOT
  :: (Has Random sig m)
  => Int
  -> Peer BookRole Book Buyer m OT S4
choiceOT _i = I.do
  At b <- liftm $ uniform @Bool
  if b
    then liftConstructor OTOne
    else liftConstructor OTTwo

buyerPeer
  :: (Has Random sig m)
  => Peer BookRole Book Buyer m (At (Maybe Date) (Done Buyer)) S0
buyerPeer = I.do
  yield (Title "haskell book")
  await I.>>= \case
    Recv FinishBuyer -> I.do
       yield FinishBuyer2
       returnAt Nothing
    Recv NoBook -> I.do
      yield SellerNoBook
      buyerPeer
    Recv (Price i) -> I.do
      choiceOT i I.>>= \case
        OTOne -> I.do
          yield OneAccept
          Recv (OneDate d) <- await
          yield (OneSuccess d)
          buyerPeer
        OTTwo -> f1
 where
  f1
    :: (Has Random sig m)
    => Peer BookRole Book 'Buyer m (At (Maybe Date) (Done Buyer)) ('S1 'Two)
  f1 = I.do
    yield (PriceToBuyer2 300)
    await I.>>= \case
      Recv NotSupport1 -> I.do
        yield TwoNotBuy
        buyerPeer
      Recv (SupportVal h) -> I.do
        checkPrice 10 h I.>>= \case
          Yes -> I.do
            yield TwoAccept
            Recv (TwoDate d) <- await
            yield (TwoSuccess d)
            buyerPeer
          No -> I.do
            yield TwoNotBuy1
            yield TwoFailed
            buyerPeer

data BuySupp :: Book -> Type where
  BNS :: BuySupp (S6 NotSupport)
  BS :: BuySupp (S6 Support)

choiceB
  :: (Has Random sig m)
  => Int
  -> Peer BookRole Book Buyer2 m BuySupp S7
choiceB _i = I.do
  At b <- liftm $ uniform @Bool
  if b
    then liftConstructor BNS
    else liftConstructor BS

buyer2Peer
  :: (Has Random sig m)
  => Peer BookRole Book Buyer2 m (At (Maybe Date) (Done Buyer2)) (S1 s)
buyer2Peer = I.do
  await I.>>= \case
    Recv FinishBuyer2 -> returnAt Nothing
    Recv SellerNoBook -> buyer2Peer
    Recv (OneSuccess d) -> buyer2Peer
    Recv (PriceToBuyer2 i) -> I.do
      choiceB i I.>>= \case
        BNS -> I.do
          yield NotSupport1
          buyer2Peer
        BS -> I.do
          yield (SupportVal (i `div` 2))
          await I.>>= \case
            Recv (TwoSuccess d) -> buyer2Peer
            Recv TwoFailed -> buyer2Peer

data FindBookResult :: Book -> Type where
  NotFound' :: FindBookResult (S2 NotFound)
  Found' :: FindBookResult (S2 Found)
  Finish' :: FindBookResult (S2 Finish)

findBook
  :: (Has (Random :+: State Int) sig m)
  => String
  -> Peer BookRole Book Seller m FindBookResult S3
findBook _st = I.do
  At i <- liftm $ get @Int
  if i > 30
    then LiftM $ pure (ireturn Finish')
    else I.do
      At b <- liftm $ uniform @Bool
      if b
        then LiftM $ pure (ireturn Found')
        else LiftM $ pure (ireturn NotFound')

sellerPeer
  :: (Has (Random :+: State Int) sig m)
  => Peer BookRole Book Seller m (At () (Done Seller)) S0
sellerPeer = I.do
  liftm $ modify @Int (+ 1)
  Recv (Title st) <- await
  findBook st I.>>= \case
    Finish' -> I.do
      yield FinishBuyer
    NotFound' -> I.do
      yield NoBook
      sellerPeer
    Found' -> I.do
      yield (Price 30)
      await I.>>= \case
        Recv OneAccept -> I.do
          yield (OneDate 100)
          sellerPeer
        Recv TwoNotBuy -> sellerPeer
        Recv TwoAccept -> I.do
          yield (TwoDate 100)
          sellerPeer
        Recv TwoNotBuy1 -> sellerPeer
