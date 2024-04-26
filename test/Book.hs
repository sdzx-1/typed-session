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

module Book where

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

--------------------------------------------------------------------------
    Buyer                                                      Seller
    :S0                                                        :S0
     <                     Title String  ->                     >
    :S1                                                        :S1
     <                     <-  Price Int                         >
    :S12'                                                       :S12 s

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
  | S12'
  | S12 BudgetSt
  | S3
  | End

data SBookSt :: BookSt -> Type where
  SS0 :: SBookSt S0
  SS1 :: SBookSt S1
  SS12' :: SBookSt S12'
  SS12 :: SBookSt (S12 (s :: BudgetSt))
  SS3 :: SBookSt S3
  SEnd :: SBookSt End

type instance Sing = SBookSt

instance SingI S0 where
  sing = SS0

instance SingI S1 where
  sing = SS1

instance SingI S12' where
  sing = SS12'

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
    Price :: Int -> Msg Role BookSt S1 '(Seller, S12 s) '(Buyer, S12')
    Afford :: Msg Role BookSt (S12 EnoughBudget) '(Buyer, S3) '(Seller, S3)
    Date :: Date -> Msg Role BookSt S3 '(Seller, End) '(Buyer, End)
    NotBuy :: Msg Role BookSt (S12 NotEnoughBuget) '(Buyer, End) '(Seller, End)

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
            (Agency SBuyer SS1, Price{}) -> DecodeDone (SomeMsg (Recv msg)) Nothing
            (Agency SBuyer SS3, Date{}) -> DecodeDone (SomeMsg (Recv msg)) Nothing
            (Agency SSeller SS0, Title{}) -> DecodeDone (SomeMsg (Recv msg)) Nothing
            (Agency SSeller SS12, Afford{}) -> DecodeDone (SomeMsg (liftRecv msg)) Nothing
            (Agency SSeller SS12, NotBuy{}) -> DecodeDone (SomeMsg (liftRecv msg)) Nothing
            _ -> error "np"

budget :: Int
budget = 100

data CheckPriceResult :: BookSt -> Type where
  Yes :: CheckPriceResult (S12 EnoughBudget)
  No :: CheckPriceResult (S12 NotEnoughBuget)

checkPrice :: Int -> Peer Role BookSt Buyer IO CheckPriceResult S12'
checkPrice i =
  if i <= budget
    then LiftM $ pure (ireturn Yes)
    else LiftM $ pure (ireturn No)

buyerPeer
  :: Peer Role BookSt Buyer IO (At (Maybe Date) (Done Buyer)) S0
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

sellerPeer :: Peer Role BookSt Seller IO (At () (Done Seller)) S0
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
    Afford -> "Afford"
    Date i -> "Date " <> show i
    NotBuy -> "NotBuy"

runAll :: IO ()
runAll = do
  buyerTMVar <- newEmptyTMVarIO @(AnyMsg Role BookSt)
  sellerTMVar <- newEmptyTMVarIO @(AnyMsg Role BookSt)

  let sendFun :: forall r. Sing (r :: Role) -> AnyMsg Role BookSt -> IO ()
      sendFun sr a = case sr of
        SBuyer -> atomically $ putTMVar buyerTMVar a
        SSeller -> atomically $ putTMVar sellerTMVar a

  let channel1 = mvarsAsChannel sellerTMVar sendFun
      channel2 = mvarsAsChannel buyerTMVar sendFun

  forkIO $ void $ do
    runPeerWithDriver (driverSimple (myTracer "Seller: ") codecRoleBookSt channel1) sellerPeer Nothing

  runPeerWithDriver (driverSimple (myTracer "Buyer: ") codecRoleBookSt channel2) buyerPeer Nothing
  pure ()
