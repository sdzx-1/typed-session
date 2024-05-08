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
{-# LANGUAGE TemplateHaskell #-}
{-# HLINT ignore "Use lambda-case" #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}

module Book1 where

import Control.Concurrent.Class.MonadSTM
import Control.Monad
import Control.Monad.Class.MonadFork (MonadFork, forkIO)
import Control.Monad.Class.MonadSay
import Control.Monad.Class.MonadThrow (MonadThrow)
import qualified Data.Dependent.Map as D
import Data.Dependent.Sum
import Data.GADT.Compare (GCompare (..), GEq (..), GOrdering (..))
import Data.GADT.Compare.TH
import Data.IFunctor (Any (..), At (..), Sing, SingI, ireturn, returnAt)
import qualified Data.IFunctor as I
import Data.Kind
import Type.Reflection
import TypedProtocol.Codec
import TypedProtocol.Core
import TypedProtocol.Driver
import Unsafe.Coerce (unsafeCoerce)

{-

-----------------------------------------------------------------------------------------------
    Buyer                                                      Seller                  Buyer2
    :S0                                                        :S0
     <                     Title String  ->                     >
    :S1                                                        :S1
     <                     <-  Price Int                         >
    :S11                                                       :S12 s                  :S11
     <                                  PriceToBuyer2 Int ->                            >
    :S110                                                                              :S110
     <                                  <- HalfPrice  Int                               >
    :S12 s                                                                             :End

   ---------------------------------------------------------------------
   |:S12 EnoughBudget                                          :S12 s
   | <                  Afford ->                               >
   |:S3                                                        :S3
   | <                  <- Data Int                             >
   |:End                                                       :End
   ---------------------------------------------------------------------

   ---------------------------------------------------------------------
   |:S12  NotEnoughBuget                                       :S12
   | <                  NotBuy ->                               >
   |:End                                                       :End
   ---------------------------------------------------------------------
-}

data Role = Buyer | Seller | Buyer2
  deriving (Show, Eq, Ord)

data SRole :: Role -> Type where
  SBuyer :: SRole Buyer
  SBuyer2 :: SRole Buyer2
  SSeller :: SRole Seller

deriveGEq ''SRole
deriveGCompare ''SRole

type instance Sing = SRole

instance SingI Buyer where
  sing = SBuyer

instance SingI Buyer2 where
  sing = SBuyer2

instance SingI Seller where
  sing = SSeller

data BudgetSt
  = EnoughBudget
  | NotEnoughBuget

data BookSt
  = S0
  | S1
  | S11
  | S110
  | S12 BudgetSt
  | S3
  | End

data SBookSt :: BookSt -> Type where
  SS0 :: SBookSt S0
  SS1 :: SBookSt S1
  SS11 :: SBookSt S11
  SS110 :: SBookSt S110
  SS12 :: SBookSt (S12 (s :: BudgetSt))
  SS3 :: SBookSt S3
  SEnd :: SBookSt End

instance GEq SBookSt where
  geq SS0 SS0 = do return Refl
  geq SS1 SS1 = do return Refl
  geq SS11 SS11 = do return Refl
  geq SS110 SS110 = do return Refl
  geq SS12 SS12 = do return $ unsafeCoerce Refl
  geq SS3 SS3 = do return Refl
  geq SEnd SEnd = do return Refl
  geq _ _ = Nothing

instance GCompare SBookSt where
  gcompare SS0 SS0 = runGComparing (do return GEQ)
  gcompare SS0{} _ = GLT
  gcompare _ SS0{} = GGT
  gcompare SS1 SS1 = runGComparing (do return GEQ)
  gcompare SS1{} _ = GLT
  gcompare _ SS1{} = GGT
  gcompare SS11 SS11 = runGComparing (do return GEQ)
  gcompare SS11{} _ = GLT
  gcompare _ SS11{} = GGT
  gcompare SS110 SS110 = runGComparing (do return GEQ)
  gcompare SS110{} _ = GLT
  gcompare _ SS110{} = GGT
  gcompare SS12 SS12 = runGComparing (do return $ unsafeCoerce GEQ)
  gcompare SS12{} _ = GLT
  gcompare _ SS12{} = GGT
  gcompare SS3 SS3 = runGComparing (do return GEQ)
  gcompare SS3{} _ = GLT
  gcompare _ SS3{} = GGT
  gcompare SEnd SEnd = runGComparing (do return GEQ)
  gcompare SEnd{} _ = GLT
  gcompare _ SEnd{} = GGT

type instance Sing = SBookSt

instance SingI S0 where
  sing = SS0

instance SingI S1 where
  sing = SS1

instance SingI S11 where
  sing = SS11

instance SingI S110 where
  sing = SS110

instance SingI (S12 s) where
  sing = SS12

instance SingI S3 where
  sing = SS3

instance SingI End where
  sing = SEnd

type Date = Int

instance Protocol Role BookSt where
  type Done Buyer = End
  type Done Seller = End
  type Done Buyer2 = End
  data Msg Role BookSt from send recv where
    Title :: String -> Msg Role BookSt S0 '(Buyer, S1) '(Seller, S1)
    Price :: Int -> Msg Role BookSt S1 '(Seller, S12 s) '(Buyer, S11)
    PriceToB2 :: Int -> Msg Role BookSt S11 '(Buyer, S110) '(Buyer2, S110)
    HalfPrice :: Int -> Msg Role BookSt S110 '(Buyer2, End) '(Buyer, S12 s)
    Afford :: Msg Role BookSt (S12 EnoughBudget) '(Buyer, S3) '(Seller, S3)
    Date :: Date -> Msg Role BookSt S3 '(Seller, End) '(Buyer, End)
    NotBuy :: Msg Role BookSt (S12 NotEnoughBuget) '(Buyer, End) '(Seller, End)

encodeMsg :: Encode Role BookSt (AnyMsg Role BookSt)
encodeMsg = Encode $ \x -> case x of
  Title{} -> AnyMsg x
  Price{} -> AnyMsg x
  PriceToB2{} -> AnyMsg x
  HalfPrice{} -> AnyMsg x
  Afford{} -> AnyMsg x
  Date{} -> AnyMsg x
  NotBuy{} -> AnyMsg x

decodeMsg
  :: forall m
   . (Monad m)
  => DecodeStep
      (AnyMsg Role BookSt)
      CodecFailure
      m
      (AnyMsg Role BookSt)
decodeMsg =
  DecodePartial $ \case
    Nothing -> return $ DecodeFail (CodecFailure "expected more data")
    Just anyMsg -> pure $ DecodeDone anyMsg Nothing

budget :: Int
budget = 16

data CheckPriceResult :: BookSt -> Type where
  Yes :: CheckPriceResult (S12 EnoughBudget)
  No :: CheckPriceResult (S12 NotEnoughBuget)

checkPrice :: (Monad m) => Int -> Int -> Peer Role BookSt Buyer m CheckPriceResult (S12 s)
checkPrice i h =
  if i <= budget + h
    then LiftM $ pure (ireturn Yes)
    else LiftM $ pure (ireturn No)

buyerPeer
  :: (Monad m) => Peer Role BookSt Buyer m (At (Maybe Date) (Done Buyer)) S0
buyerPeer = I.do
  yield (Title "haskell book")
  Recv (Price i) <- await
  yield (PriceToB2 i)
  Recv (HalfPrice hv) <- await
  res <- checkPrice i hv
  case res of
    Yes -> I.do
      yield Afford
      Recv (Date d) <- await
      returnAt (Just d)
    No -> I.do
      yield NotBuy
      returnAt Nothing

buyer2Peer
  :: (Monad m) => Peer Role BookSt Buyer2 m (At () (Done Buyer2)) S11
buyer2Peer = I.do
  Recv (PriceToB2 i) <- await
  yield (HalfPrice (i `div` 2))

sellerPeer :: (Monad m) => Peer Role BookSt Seller m (At () (Done Seller)) S0
sellerPeer = I.do
  Recv (Title _name) <- await
  yield (Price 30)
  Recv msg <- await
  case msg of
    Afford -> yield (Date 100)
    NotBuy -> returnAt ()

mvarsAsChannel
  :: (MonadSTM m)
  => TMVar m a
  -> TMVar m a
  -> Channel m a
mvarsAsChannel bufferRead bufferWrite =
  Channel{send, recv}
 where
  send x = atomically (putTMVar bufferWrite x)
  recv = atomically (Just <$> takeTMVar bufferRead)

myTracer :: (MonadSay m) => String -> Tracer Role BookSt m
myTracer st v = say (st <> show v)

instance Show (AnyMsg Role BookSt) where
  show (AnyMsg msg) = case msg of
    Title st -> "Title " <> show st
    Price i -> "Price " <> show i
    PriceToB2 i -> "PriceToB2 " <> show i
    HalfPrice i -> "HalfPrice " <> show i
    Afford -> "Afford"
    Date i -> "Date " <> show i
    NotBuy -> "NotBuy"

runAll :: forall m. (Monad m, MonadSTM m, MonadSay m, MonadFork m, MonadThrow m) => m ()
runAll = do
  buyerTMVar <- newEmptyTMVarIO @m @(AnyMsg Role BookSt)
  buyer2TMVar <- newEmptyTMVarIO @m @(AnyMsg Role BookSt)
  sellerTMVar <- newEmptyTMVarIO @m @(AnyMsg Role BookSt)
  let buyerSellerChannel = mvarsAsChannel @m buyerTMVar sellerTMVar
      buyerBuyer2Channel = mvarsAsChannel @m buyerTMVar buyer2TMVar

      sellerBuyerChannel = mvarsAsChannel @m sellerTMVar buyerTMVar

      buyer2BuyerChannel = mvarsAsChannel @m buyer2TMVar buyerTMVar

      sendFun bufferWrite x = atomically (putTMVar bufferWrite x)
      sendToRole =
        D.fromList
          [ SSeller :=> Any (sendFun sellerTMVar)
          , SBuyer :=> Any (sendFun buyerTMVar)
          , SBuyer2 :=> Any (sendFun buyer2TMVar)
          ]
  buyerTvar <- newTVarIO D.empty
  buyer2Tvar <- newTVarIO D.empty
  sellerTvar <- newTVarIO D.empty
  let buyerDriver = driverSimple (myTracer "buyer") encodeMsg sendToRole buyerTvar
      buyer2Driver = driverSimple (myTracer "buyer2") encodeMsg sendToRole buyer2Tvar
      sellerDriver = driverSimple (myTracer "seller") encodeMsg sendToRole sellerTvar
  -- fork buyer decode thread, seller -> buyer
  forkIO $ decodeLoop (myTracer "buyer") Nothing (Decode decodeMsg) buyerSellerChannel buyerTvar
  -- fork buyer decode thread, buyer2 -> buyer
  forkIO $ decodeLoop (myTracer "buyer") Nothing (Decode decodeMsg) buyerBuyer2Channel buyerTvar

  -- fork seller decode thread, buyer -> seller
  forkIO $ decodeLoop (myTracer "seller") Nothing (Decode decodeMsg) sellerBuyerChannel sellerTvar

  -- fork buyer2 decode thread, buyer -> buyer2
  forkIO $ decodeLoop (myTracer "buyer2") Nothing (Decode decodeMsg) buyer2BuyerChannel buyer2Tvar

  -- fork seller Peer thread
  forkIO $ void $ runPeerWithDriver sellerDriver sellerPeer

  -- fork buyer2 Peer thread
  forkIO $ void $ runPeerWithDriver buyer2Driver buyer2Peer
  -- run buyer Peer
  void $ runPeerWithDriver buyerDriver buyerPeer
