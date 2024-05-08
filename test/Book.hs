{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
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

module Book where

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

--------------------------------------------------------------------------
    Buyer                                                      Seller
    :S0                                                        :S0
     <                     Title String  ->                     >
    :S1                                                        :S1
     <                     <-  Price Int                         >
    :S12 s                                                     :S12 s

   ---------------------------------------------------------------------
   |:S12 EnoughBudget                                          :S12 s
   | <                  Afford ->                               >
   |:S3                                                        :S3
   | <                  <- Data Int                             >
   |:End                                                       :End
   ---------------------------------------------------------------------

   ---------------------------------------------------------------------
   |:S12 NotEnoughBuget                                        :S12 s
   | <                  NotBuy ->                               >
   |:End                                                       :End
   ---------------------------------------------------------------------
 -}

data Role = Buyer | Seller
  deriving (Show, Eq, Ord)

data SRole :: Role -> Type where
  SBuyer :: SRole Buyer
  SSeller :: SRole Seller

deriveGEq ''SRole
deriveGCompare ''SRole

type instance Sing = SRole

instance SingI Buyer where
  sing = SBuyer

instance SingI Seller where
  sing = SSeller

data BudgetSt
  = EnoughBudget
  | NotEnoughBuget

data BookSt
  = S0
  | S1
  | S12 BudgetSt
  | S3
  | End

data SBookSt :: BookSt -> Type where
  SS0 :: SBookSt S0
  SS1 :: SBookSt S1
  SS12 :: SBookSt (S12 (s :: BudgetSt))
  SS3 :: SBookSt S3
  SEnd :: SBookSt End

instance GEq SBookSt where
  geq SS0 SS0 = do return Refl
  geq SS1 SS1 = do return Refl
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

  data Msg Role BookSt from send recv where
    Title :: String -> Msg Role BookSt S0 '(Buyer, S1) '(Seller, S1)
    Price :: Int -> Msg Role BookSt S1 '(Seller, S12 s) '(Buyer, S12 s)
    Afford :: Msg Role BookSt (S12 EnoughBudget) '(Buyer, S3) '(Seller, S3)
    Date :: Date -> Msg Role BookSt S3 '(Seller, End) '(Buyer, End)
    NotBuy :: Msg Role BookSt (S12 NotEnoughBuget) '(Buyer, End) '(Seller, End)

encodeMsg :: Encode Role BookSt (AnyMsg Role BookSt)
encodeMsg = Encode $ \x -> case x of
  Title{} -> AnyMsg x
  Price{} -> AnyMsg x
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
  DecodePartial $ \mb ->
    case mb of
      Nothing -> return $ DecodeFail (CodecFailure "expected more data")
      Just anyMsg -> pure $ DecodeDone anyMsg Nothing

budget :: Int
budget = 100

data CheckPriceResult :: BookSt -> Type where
  Yes :: CheckPriceResult (S12 EnoughBudget)
  No :: CheckPriceResult (S12 NotEnoughBuget)

checkPrice :: (Monad m) => Int -> Peer Role BookSt Buyer m CheckPriceResult (S12 s)
checkPrice i =
  if i <= budget
    then LiftM $ pure (ireturn Yes)
    else LiftM $ pure (ireturn No)

buyerPeer
  :: (Monad m) => Peer Role BookSt Buyer m (At (Maybe Date) (Done Buyer)) S0
buyerPeer = I.do
  yield (Title "haskell book")
  Recv (Price i) <- await
  res <- checkPrice i
  case res of
    Yes -> I.do
      yield Afford
      Recv (Date d) <- await
      returnAt (Just d)
    No -> I.do
      yield NotBuy
      returnAt Nothing

sellerPeer :: (Functor m) => Peer Role BookSt Seller m (At () (Done Seller)) S0
sellerPeer = I.do
  Recv (Title _name) <- await
  yield (Price 30)
  Recv msg <- await
  case msg of
    Afford -> I.do
      yield (Date 100)
    NotBuy -> I.do
      returnAt ()

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
    Afford -> "Afford"
    Date i -> "Date " <> show i
    NotBuy -> "NotBuy"

runAll :: forall m. (Monad m, MonadSTM m, MonadSay m, MonadFork m, MonadThrow m) => m ()
runAll = do
  buyerTMVar <- newEmptyTMVarIO @m @(AnyMsg Role BookSt)
  sellerTMVar <- newEmptyTMVarIO @m @(AnyMsg Role BookSt)
  let buyerChannel = mvarsAsChannel @m buyerTMVar sellerTMVar
      sellerChannel = mvarsAsChannel @m sellerTMVar buyerTMVar
      sendFun bufferWrite x = atomically (putTMVar bufferWrite x)
      sendToRole =
        D.fromList
          [ SSeller :=> Any (sendFun sellerTMVar)
          , SBuyer :=> Any (sendFun buyerTMVar)
          ]
  buyerTvar <- newTVarIO D.empty
  sellerTvar <- newTVarIO D.empty
  let buyerDriver = driverSimple (myTracer "buyer") encodeMsg sendToRole buyerTvar
      sellerDriver = driverSimple (myTracer "seller") encodeMsg sendToRole sellerTvar
  -- fork buyer decode thread, seller -> buyer
  forkIO $ decodeLoop (myTracer "buyer") Nothing (Decode decodeMsg) buyerChannel buyerTvar
  -- fork seller decode thread, buyer -> seller
  forkIO $ decodeLoop (myTracer "seller") Nothing (Decode decodeMsg) sellerChannel sellerTvar
  -- fork seller Peer thread
  forkIO $ void $ runPeerWithDriver sellerDriver sellerPeer
  -- run buyer Peer
  void $ runPeerWithDriver buyerDriver buyerPeer
