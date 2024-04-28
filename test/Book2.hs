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
{-# HLINT ignore "Use lambda-case" #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}

module Book2 where

import Control.Concurrent (forkIO)
import Control.Concurrent.STM
import Control.Monad
import Data.IFunctor (At (..), Sing, SingI, ireturn, returnAt)
import qualified Data.IFunctor as I
import Data.Kind
import TypedProtocol.Codec
import TypedProtocol.Core
import TypedProtocol.Driver

{-

-----------------------------------------------------------------------------------------------
    Buyer                                                      Seller                  Buyer2
    :S0                                                        :S0                      :S11 s
     <                     Title String  ->                     >
    :S1 s                                                      :S1 s

 ------------------------------------------------------------------------------------------
 |  :S1 s                                                      :S1 BookNotFound
 |   <                     <-  BookNotFound                     >
 |  :S11 BookNotFound                                          :End                    :S11 s
 |   <                                  SellerNotFoundBook ->                            >
 |  :End                                                                               :End
 ------------------------------------------------------------------------------------------

 ------------------------------------------------------------------------------------------
 |  :S1 s                                                      :S1 BookFound
 |   <                     <-  Price Int                         >
 |  :S11 BookFound                                             :S12 s                  :S11 s
 |   <                                  PriceToBuyer2 Int ->                            >
 |  :S110                                                                              :S110
 |   <                                  <- HalfPrice  Int                               >
 |  :S12 s                                                                             :S113 s
 |
 | ----------------------------------------------------------------------------------------
 | |:S12  EnoughBudget                                         :S12 s
 | | <                  Afford ->                               >
 | |:S3                                                        :S3
 | | <                  <- Date Int                             >
 | | :S113 EnoughtBudget                                      :End                   :S113 s
 | | <                                 Success Int  ->                                  >
 | |:End                                                                              :End
 | ----------------------------------------------------------------------------------------
 |
 | ----------------------------------------------------------------------------------------
 | |:S12  NotEnoughBuget                                       :S12 s
 | | <                  NotBuy ->                               >
 | | S113 NotEnoughBuget                                      :End                  :S113 s
 | | <                                 Failed  ->                                       >
 | |:End                                                                             :End
 | ----------------------------------------------------------------------------------------
 ------------------------------------------------------------------------------------------
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
  | S11 FindBook
  | S110
  | S113 BudgetSt
  | S12 BudgetSt
  | S3
  | End

data SBookSt :: BookSt -> Type where
  SS0 :: SBookSt S0
  SS1 :: SBookSt (S1 (s :: FindBook))
  SS11 :: SBookSt (S11 (s :: FindBook))
  SS110 :: SBookSt S110
  SS113 :: SBookSt (S113 (s :: BudgetSt))
  SS12 :: SBookSt (S12 (s :: BudgetSt))
  SS3 :: SBookSt S3
  SEnd :: SBookSt End

type instance Sing = SBookSt

instance SingI S0 where
  sing = SS0

instance SingI (S1 s) where
  sing = SS1

instance SingI (S11 s) where
  sing = SS11

instance SingI S110 where
  sing = SS110

instance SingI (S113 s) where
  sing = SS113

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
    Title :: String -> Msg Role BookSt S0 '(Buyer, S1 s) '(Seller, S1 s)
    Price :: Int -> Msg Role BookSt (S1 BookFound) '(Seller, S12 s) '(Buyer, S11 BookFound)
    PriceToB2 :: Int -> Msg Role BookSt (S11 BookFound) '(Buyer, S110) '(Buyer2, S110)
    HalfPrice :: Int -> Msg Role BookSt S110 '(Buyer2, S113 s) '(Buyer, S12 s)
    Afford :: Msg Role BookSt (S12 EnoughBudget) '(Buyer, S3) '(Seller, S3)
    Date :: Date -> Msg Role BookSt S3 '(Seller, End) '(Buyer, S113 EnoughBudget)
    Success :: Int -> Msg Role BookSt (S113 EnoughBudget) '(Buyer, End) '(Buyer2, End)
    NotBuy :: Msg Role BookSt (S12 NotEnoughBuget) '(Buyer, S113 NotEnoughBuget) '(Seller, End)
    Failed :: Msg Role BookSt (S113 NotEnoughBuget) '(Buyer, End) '(Buyer2, End)
    BookNotFoun :: Msg Role BookSt (S1 BookNotFound) '(Seller, End) '(Buyer, S11 BookNotFound)
    SellerNotFoundBook :: Msg Role BookSt (S11 BookNotFound) '(Buyer, End) '(Buyer2, End)

codecRoleBookSt
  :: forall m
   . (Monad m)
  => Codec Role BookSt CodecFailure m (AnyMsg Role BookSt)
codecRoleBookSt = Codec{encode, decode}
 where
  encode _ = AnyMsg
  decode
    :: forall (r :: Role) (from :: BookSt)
     . Agency Role BookSt r from
    -> m
        ( DecodeStep
            (AnyMsg Role BookSt)
            CodecFailure
            m
            (SomeMsg Role BookSt r from)
        )
  decode stok =
    pure $ DecodePartial $ \mb ->
      case mb of
        Nothing -> return $ DecodeFail (CodecFailure "expected more data")
        Just (AnyMsg msg) -> return $
          case (stok, msg) of
            (Agency SSeller SS0, Title{}) -> DecodeDone (SomeMsg (Recv msg)) Nothing
            (Agency SBuyer SS1, Price{}) -> DecodeDone (SomeMsg (liftRecv msg)) Nothing
            (Agency SBuyer2 SS11, PriceToB2{}) -> DecodeDone (SomeMsg (liftRecv msg)) Nothing
            (Agency SBuyer SS110, HalfPrice{}) -> DecodeDone (SomeMsg (Recv msg)) Nothing
            ------------------------
            (Agency SSeller SS12, Afford{}) -> DecodeDone (SomeMsg (liftRecv msg)) Nothing
            (Agency SBuyer SS3, Date{}) -> DecodeDone (SomeMsg (Recv msg)) Nothing
            (Agency SBuyer2 SS113, Success{}) -> DecodeDone (SomeMsg (liftRecv msg)) Nothing
            ------------------------
            (Agency SSeller SS12, NotBuy{}) -> DecodeDone (SomeMsg (liftRecv msg)) Nothing
            (Agency SBuyer2 SS113, Failed{}) -> DecodeDone (SomeMsg (liftRecv msg)) Nothing
            ------------------------
            (Agency SBuyer SS1, BookNotFoun{}) -> DecodeDone (SomeMsg (liftRecv msg)) Nothing
            (Agency SBuyer2 SS11, SellerNotFoundBook{}) -> DecodeDone (SomeMsg (liftRecv msg)) Nothing
            _ -> error "np"

budget :: Int
budget = 16

data CheckPriceResult :: BookSt -> Type where
  Yes :: CheckPriceResult (S12 EnoughBudget)
  No :: CheckPriceResult (S12 NotEnoughBuget)

checkPrice :: Int -> Int -> Peer Role BookSt Buyer IO CheckPriceResult (S12 s)
checkPrice i h =
  if i <= budget + h
    then LiftM $ pure (ireturn Yes)
    else LiftM $ pure (ireturn No)

buyerPeer
  :: Peer Role BookSt Buyer IO (At (Maybe Date) (Done Buyer)) S0
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

buyerPeer2
  :: Peer Role BookSt Buyer2 IO (At () (Done Buyer2)) (S11 s)
buyerPeer2 = I.do
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
  Found :: FindBookResult (S1 BookFound)
  NotFound :: FindBookResult (S1 BookNotFound)

findBook :: String -> Peer Role BookSt Seller IO FindBookResult (S1 s)
findBook st =
  if st /= ""
    then LiftM $ pure (ireturn Found)
    else LiftM $ pure (ireturn NotFound)

sellerPeer :: Peer Role BookSt Seller IO (At () (Done Seller)) S0
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

newTMV :: s -> IO (s, TMVar a)
newTMV s = do
  ntmv <- newEmptyTMVarIO
  pure (s, ntmv)

mvarsAsChannel
  :: TMVar a
  -> (forall r. Sing (r :: role') -> (a -> IO ()))
  -> Channel role' IO a
mvarsAsChannel bufferRead sendFun =
  Channel{sendFun, recv}
 where
  recv = atomically (Just <$> takeTMVar bufferRead)

myTracer :: String -> Tracer Role BookSt IO
myTracer st v = putStrLn (st <> show v)

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

runAll :: IO ()
runAll = do
  buyerTMVar <- newEmptyTMVarIO @(AnyMsg Role BookSt)
  buyer2TMVar <- newEmptyTMVarIO @(AnyMsg Role BookSt)
  sellerTMVar <- newEmptyTMVarIO @(AnyMsg Role BookSt)

  let sendFun :: forall r. Sing (r :: Role) -> AnyMsg Role BookSt -> IO ()
      sendFun sr a = case sr of
        SBuyer -> atomically $ putTMVar buyerTMVar a
        SBuyer2 -> atomically $ putTMVar buyer2TMVar a
        SSeller -> atomically $ putTMVar sellerTMVar a

  let chanSeller = mvarsAsChannel sellerTMVar sendFun
      chanBuyer2 = mvarsAsChannel buyer2TMVar sendFun
      chanBuyer = mvarsAsChannel buyerTMVar sendFun

  forkIO $ void $ do
    runPeerWithDriver (driverSimple (myTracer "Seller: ") codecRoleBookSt chanSeller) sellerPeer Nothing

  forkIO $ void $ do
    runPeerWithDriver (driverSimple (myTracer "Buyer2: ") codecRoleBookSt chanBuyer2) buyerPeer2 Nothing

  runPeerWithDriver (driverSimple (myTracer "Buyer: ") codecRoleBookSt chanBuyer) buyerPeer Nothing
  pure ()
