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
{-# OPTIONS_GHC -Wno-unused-do-bind #-}

module Book3.Peer where

import Book3.Protocol
import Book3.Type
import Control.Carrier.Lift
import Control.Carrier.Random.Gen
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
  => Peer BookRole Book Buyer m (At (Maybe Date) Done) BuyerStartSt
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
  => Peer BookRole Book Buyer2 m (At (Maybe Date) Done) (Buyer2StartSt s)
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
  => Peer BookRole Book Seller m (At () Done) SellerStartSt
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

-- data AnyPeer role' ps m where
--   AnyPeer :: Peer role' ps r m (At () (Done r)) st -> AnyPeer role' ps m

-- runAnyPeers
--   :: forall n role' ps sig m
--    . ( SingToInt role'
--      , Has (State (IntMap (AnyPeer role' ps n))) sig m
--      )
--   => (forall a. n a -> m a) -> m ()
-- runAnyPeers liftFun = do
--   im <- get @(IntMap (AnyPeer role' ps n))
--   case IntMap.keys im of
--     [] -> pure ()
--     keys -> do
--       forM_ keys $ \key -> do
--         gets @(IntMap (AnyPeer role' ps n)) (IntMap.lookup key) >>= \case
--           Nothing -> error "np"
--           Just (AnyPeer peer) -> case peer of
--             IReturn (At ()) -> do
--               modify @(IntMap (AnyPeer role' ps n)) (IntMap.delete key)
--             LiftM fm -> do
--               np <- liftFun fm
--               modify @(IntMap (AnyPeer role' ps n)) (IntMap.insert key (AnyPeer np))
--             Yield (msg :: Msg role' ps st send sps recv rps) cont -> do
--               let recvKey = (singToInt $ sing @recv)
--               gets @(IntMap (AnyPeer role' ps n)) (IntMap.lookup recvKey) >>= \case
--                 Nothing -> error "np"
--                 Just (AnyPeer recvPeer) -> case recvPeer of
--                   Await scont -> do
--                     let nS = scont (unsafeCoerce msg)
--                     modify @(IntMap (AnyPeer role' ps n)) (IntMap.insert recvKey (AnyPeer nS))
--                   _ -> error "np"
--               modify @(IntMap (AnyPeer role' ps n)) (IntMap.insert key (AnyPeer cont))
--             Await{} -> pure ()
--       runAnyPeers @n @role' @ps liftFun

-- runAP = do
--   i <- randomIO @Int
--   runRandom (mkStdGen i)
--     $ runM @(RandomC StdGen IO)
--     $ runState
--       ( IntMap.fromList
--           [ (singToInt SBuyer, AnyPeer (buyerPeer @_ @(RandomC StdGen IO) I.>> returnAt ()))
--           , (singToInt SBuyer2, AnyPeer (buyer2Peer @_ @(RandomC StdGen IO) I.>> returnAt ()))
--           , (singToInt SSeller, AnyPeer (sellerPeer @_ @(RandomC StdGen IO) I.>> returnAt ()))
--           ]
--       )
--     $ (runAnyPeers @(RandomC StdGen IO) @BookRole @Book sendM)
