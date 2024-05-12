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
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Book2 where

import Control.Concurrent.Class.MonadSTM
import Control.Monad
import Control.Monad.Class.MonadFork (MonadFork, forkIO)
import Control.Monad.Class.MonadSay
import Control.Monad.Class.MonadThrow (MonadThrow)
import Data.IFunctor (At (..), Sing, SingI, ireturn, returnAt)
import qualified Data.IFunctor as I
import qualified Data.IntMap as IntMap
import Data.Kind
import GHC.Exts (dataToTag#)
import GHC.Int (Int (I#))
import TypedProtocol.Codec
import TypedProtocol.Core
import TypedProtocol.Driver

{-

-----------------Buyer---------------Seller--------------Buyer2-----------------
                   S0                  S0                 S1 s
       Title       |        --->       |
                  S2 s                S2 s                S1 s
    ------------------------------BookNotFound------------------------------
                  S2 s          S2 BookNotFound           S1 s
    BookNotFoun    |        <---       |
            S1 BookNotFound           End                 S1 s
  SellerNotFoundB  |                  --->                 |
                  End                 End                 End
                                    Terminal

    -------------------------------BookFound--------------------------------
                  S2 s            S2 BookFound            S1 s
       Price       |        <---       |
              S1 BookFound            S3 s                S1 s
   PriceToBuyer2   |                  --->                 |
                   S4                 S3 s                 S4
     HalfPrice     |                  <---                 |
                  S3 s                S3 s                S5 s
        --------------------------EnoughBudget--------------------------
            S3 EnoughBudget           S3 s                S5 s
       Afford      |        --->       |
                   S6                  S6                 S5 s
        Data       |        <---       |
            S5 EnoughBudget           End                 S5 s
      Success      |                  --->                 |
                  End                 End                 End
                                    Terminal

        ------------------------NotEnoughBudget-------------------------
           S3 NotEnoughBudget         S3 s                S5 s
       NotBuy      |        --->       |
           S5 NotEnoughBudget         End                 S5 s
       Failed      |                  --->                 |
                  End                 End                 End
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

data FindBook
  = BookNotFound
  | BookFound

data BudgetSt
  = EnoughBudget
  | NotEnoughBuget

data BookSt
  = S0
  | S1 FindBook
  | S2 FindBook
  | S3 BudgetSt
  | S4
  | S5 BudgetSt
  | S6
  | End

data SBookSt :: BookSt -> Type where
  SS0 :: SBookSt S0
  SS1 :: SBookSt (S1 (s :: FindBook))
  SS2 :: SBookSt (S2 (s :: FindBook))
  SS3 :: SBookSt (S3 (s :: BudgetSt))
  SS4 :: SBookSt S4
  SS5 :: SBookSt (S5 (s :: BudgetSt))
  SS6 :: SBookSt S6
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

instance SingI (S5 s) where
  sing = SS5

instance SingI S6 where
  sing = SS6

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
    Price :: Int -> Msg Role BookSt (S2 BookFound) '(Seller, S3 s) '(Buyer, S1 BookFound)
    PriceToB2 :: Int -> Msg Role BookSt (S1 BookFound) '(Buyer, S4) '(Buyer2, S4)
    HalfPrice :: Int -> Msg Role BookSt S4 '(Buyer2, S5 s) '(Buyer, S3 s)
    Afford :: Msg Role BookSt (S3 EnoughBudget) '(Buyer, S6) '(Seller, S6)
    Date :: Date -> Msg Role BookSt S6 '(Seller, End) '(Buyer, S5 EnoughBudget)
    Success :: Int -> Msg Role BookSt (S5 EnoughBudget) '(Buyer, End) '(Buyer2, End)
    NotBuy :: Msg Role BookSt (S3 NotEnoughBuget) '(Buyer, S5 NotEnoughBuget) '(Seller, End)
    Failed :: Msg Role BookSt (S5 NotEnoughBuget) '(Buyer, End) '(Buyer2, End)
    BookNotFoun :: Msg Role BookSt (S2 BookNotFound) '(Seller, End) '(Buyer, S1 BookNotFound)
    SellerNotFoundBook :: Msg Role BookSt (S1 BookNotFound) '(Buyer, End) '(Buyer2, End)

encodeMsg :: Encode Role BookSt (AnyMsg Role BookSt)
encodeMsg = Encode $ \x -> case x of
  Title{} -> AnyMsg x
  Price{} -> AnyMsg x
  PriceToB2{} -> AnyMsg x
  HalfPrice{} -> AnyMsg x
  Afford{} -> AnyMsg x
  Date{} -> AnyMsg x
  Success{} -> AnyMsg x
  NotBuy{} -> AnyMsg x
  Failed{} -> AnyMsg x
  BookNotFoun{} -> AnyMsg x
  SellerNotFoundBook{} -> AnyMsg x

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
  Yes :: CheckPriceResult (S3 EnoughBudget)
  No :: CheckPriceResult (S3 NotEnoughBuget)

checkPrice :: (Monad m) => Int -> Int -> Peer Role BookSt Buyer m CheckPriceResult (S3 s)
checkPrice i h =
  if i <= budget + h
    then LiftM $ pure (ireturn Yes)
    else LiftM $ pure (ireturn No)

buyerPeer
  :: (Monad m) => Peer Role BookSt Buyer m (At (Maybe Date) (Done Buyer)) S0
buyerPeer = I.do
  yield (Title "haskell book")
  Recv msg <- await
  case msg of
    BookNotFoun -> I.do
      yield SellerNotFoundBook
      returnAt Nothing
    Price i -> I.do
      yield (PriceToB2 i)
      Recv (HalfPrice hv) <- await
      res <- checkPrice i hv
      case res of
        Yes -> I.do
          yield Afford
          Recv (Date d) <- await
          yield (Success d)
          returnAt (Just d)
        No -> I.do
          yield NotBuy
          yield Failed
          returnAt Nothing

buyer2Peer
  :: (Monad m) => Peer Role BookSt Buyer2 m (At () (Done Buyer2)) (S1 s)
buyer2Peer = I.do
  Recv msg' <- await
  case msg' of
    SellerNotFoundBook -> returnAt ()
    PriceToB2 i -> I.do
      yield (HalfPrice (i `div` 2))
      Recv msg <- await
      case msg of
        Success _ -> returnAt ()
        Failed -> returnAt ()

data FindBookResult :: BookSt -> Type where
  Found :: FindBookResult (S2 BookFound)
  NotFound :: FindBookResult (S2 BookNotFound)

findBook :: (Monad m) => String -> Peer Role BookSt Seller m FindBookResult (S2 s)
findBook st =
  if st /= ""
    then LiftM $ pure (ireturn Found)
    else LiftM $ pure (ireturn NotFound)

sellerPeer :: (Monad m) => Peer Role BookSt Seller m (At () (Done Seller)) S0
sellerPeer = I.do
  Recv (Title name) <- await
  res <- findBook name
  case res of
    Found -> I.do
      yield (Price 30)
      Recv msg <- await
      case msg of
        Afford -> yield (Date 100)
        NotBuy -> returnAt ()
    NotFound -> yield BookNotFoun

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
    Success i -> "Success " <> show i
    Failed -> "Failed"
    BookNotFoun -> "BookNotFound"
    SellerNotFoundBook -> "SellerNotFoundBook"

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
        IntMap.fromList
          [ (singToInt SSeller, sendFun sellerTMVar)
          , (singToInt SBuyer, sendFun buyerTMVar)
          , (singToInt SBuyer2, sendFun buyer2TMVar)
          ]
  buyerTvar <- newTVarIO IntMap.empty
  buyer2Tvar <- newTVarIO IntMap.empty
  sellerTvar <- newTVarIO IntMap.empty
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
