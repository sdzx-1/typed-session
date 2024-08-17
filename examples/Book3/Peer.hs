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

module Peer where

import Control.Algebra (Has)
import Control.Effect.Random (Random, uniform)
import Data.IFunctor (At (..), ireturn, returnAt)
import qualified Data.IFunctor as I
import Data.Kind
import Type
import TypedProtocol.Core

budget :: Int
budget = 16

data CheckPriceResult :: BookSt -> Type where
  Yes :: CheckPriceResult (S3 Enough)
  No :: CheckPriceResult (S3 NotEnough)

checkPrice
  :: (Has Random sig m)
  => Int
  -> Int
  -> Peer Role BookSt Buyer m CheckPriceResult (S3 s)
checkPrice _i _h = I.do
  At b <- liftm $ uniform @Bool
  if b
    then LiftM $ pure (ireturn Yes)
    else LiftM $ pure (ireturn No)

data OT :: BookSt -> Type where
  OTOne :: OT (S1 One)
  OTTwo :: OT (S1 Two)

choiceOT
  :: (Has Random sig m)
  => Int
  -> Peer Role BookSt Buyer m OT (S1 s)
choiceOT _i = I.do
  At b <- liftm $ uniform @Bool
  if b
    then LiftM $ pure $ ireturn OTOne
    else LiftM $ pure $ ireturn OTTwo

buyerPeer
  :: (Has Random sig m)
  => Peer Role BookSt Buyer m (At (Maybe Int) (Done Buyer)) S0
buyerPeer = I.do
  yield (Title "haskell book")
  await I.>>= \case
    Recv NoBook -> I.do
      yield SellerNoBook
      buyerPeer
    Recv (Price i) -> I.do
      choiceOT i I.>>= \case
        OTOne -> I.do
          yield OneAfford
          yield OneAccept
          Recv (OneDate d) <- await
          yield (OneSuccess d)
          buyerPeer
        OTTwo -> f1
 where
  f1
    :: (Has Random sig m)
    => Peer Role BookSt 'Buyer m (At (Maybe Int) (Done Buyer)) ('S1 'Two)
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
            returnAt Nothing

data BuySupp :: BookSt -> Type where
  BNS :: BuySupp (S6 'NotSupport)
  BS :: BuySupp (S6 'Support)

choiceB
  :: (Has Random sig m)
  => Int
  -> Peer Role BookSt Buyer2 m BuySupp (S6 s)
choiceB _i = I.do
  At b <- liftm $ uniform @Bool
  if b
    then LiftM $ pure $ ireturn BNS
    else LiftM $ pure $ ireturn BS

buyer2Peer
  :: (Has Random sig m)
  => Peer Role BookSt Buyer2 m (At (Maybe Int) (Done Buyer2)) (S1 s)
buyer2Peer = I.do
  await I.>>= \case
    Recv SellerNoBook -> buyer2Peer
    Recv OneAfford -> I.do
      Recv (OneSuccess d) <- await
      buyer2Peer
    Recv (PriceToBuyer2 i) -> I.do
      choiceB i I.>>= \case
        BNS -> I.do
          yield NotSupport1
          buyer2Peer
        BS -> I.do
          yield (SupportVal (i `div` 2))
          await I.>>= \case
            Recv (TwoSuccess d) -> buyer2Peer
            Recv TwoFailed -> returnAt Nothing

data FindBookResult :: BookSt -> Type where
  NotFound' :: FindBookResult (S2 'NotFound)
  Found' :: FindBookResult (S2 'Found)

findBook
  :: (Has Random sig m)
  => String
  -> Peer Role BookSt Seller m FindBookResult (S2 s)
findBook _st = I.do
  At b <- liftm $ uniform @Bool
  if b
    then LiftM $ pure (ireturn Found')
    else LiftM $ pure (ireturn NotFound')

sellerPeer
  :: (Has Random sig m)
  => Peer Role BookSt Seller m (At () (Done Seller)) S0
sellerPeer = I.do
  Recv (Title st) <- await
  findBook st I.>>= \case
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
        Recv TwoNotBuy1 -> returnAt ()
