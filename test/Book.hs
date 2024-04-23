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
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}

{-# HLINT ignore "Use lambda-case" #-}

module Book where

import Control.Concurrent (forkIO)
import Control.Concurrent.STM
import Control.Exception (throwIO)
import Control.Monad
import Data.IFunctor (At (..), ireturn, returnAt)
import qualified Data.IFunctor as I
import Data.Kind
import Data.SR
import TypedProtocol.Codec
import TypedProtocol.Core
import TypedProtocol.Driver
import Unsafe.Coerce (unsafeCoerce)
import Type

{-

-----------------------------------------------
 role: Buyer Seller

 Buyer                            Seller
       title  ->
       <-  price
       checkPrice
            CheckTrue    Afford ->
                         data <-

            CheckFalse   NotBuy ->

-----------------------------------------------
 Buyer                                                      Seller
  :S0                                                        :S0
       Title  ->
  :S1                                                        :S1
       <-  Price

  :S12
                                                            :S2 s

                     [S2 True]   Afford ->
     :S3                                             :S3
                      Data <-
     :End                                            :End

                       [S2 False]   NotBuy ->
     :End                                            :End
 - -}

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

instance Reify Buyer where
  reifyProxy _ = Buyer

instance Reify Seller where
  reifyProxy _ = Seller

data BookSt
  = S0
  | S1
  | S12
  | S2 Bool
  | S3
  | End

data SBookSt :: BookSt -> Type where
  SS0 :: SBookSt S0
  SS1 :: SBookSt S1
  SS12 :: SBookSt S12
  SS2 :: SBookSt (S2 (b :: Bool))
  SS3 :: SBookSt S3
  SEnd :: SBookSt End

type instance Sing = SBookSt

instance SingI S0 where
  sing = SS0

instance SingI S1 where
  sing = SS1

instance SingI S12 where
  sing = SS12

instance SingI (S2 s) where
  sing = SS2

instance SingI S3 where
  sing = SS3

instance SingI End where
  sing = SEnd

type Date = Int

instance Protocol Role BookSt where
  type Done Buyer = End
  type Done Seller = End

  data Msg Role BookSt send recv from to where
    Title :: String -> Msg Role BookSt Buyer Seller S0 '(S1, S1)
    Price :: Int -> Msg Role BookSt Seller Buyer S1 '(S2 s, S12)
    Afford :: Msg Role BookSt Buyer Seller (S2 True) '(S3, S3)
    Date :: Date -> Msg Role BookSt Seller Buyer S3 '(End, End)
    NotBuy :: Msg Role BookSt Buyer Seller (S2 False) '(End, End)

codecRoleBookSt
  :: forall m
   . (Monad m)
  => Codec Role BookSt CodecFailure m (AnyMessage Role BookSt)
codecRoleBookSt = Codec{encode, decode}
 where
  encode _ = AnyMessage
  decode
    :: forall (r :: Role) (from :: BookSt)
     . Agency Role BookSt r from
    -> m
        ( DecodeStep
            (AnyMessage Role BookSt)
            CodecFailure
            m
            (SomeMsg Role BookSt r from)
        )
  decode stok =
    pure $ DecodePartial $ \mb ->
      case mb of
        Nothing -> return $ DecodeFail (CodecFailure "expected more data")
        Just (AnyMessage msg) -> return $
          case (stok, msg) of
            (Agency _ SBuyer SS1, Price{}) -> DecodeDone (SomeMsg (Recv msg)) Nothing
            (Agency _ SBuyer SS3, Date{}) -> DecodeDone (SomeMsg (Recv msg)) Nothing
            (Agency _ SSeller SS0, Title{}) -> DecodeDone (SomeMsg (Recv msg)) Nothing
            (Agency _ SSeller SS2, Afford{}) -> DecodeDone (SomeMsg (Recv (unsafeCoerce msg))) Nothing
            (Agency _ SSeller SS2, NotBuy{}) -> DecodeDone (SomeMsg (Recv (unsafeCoerce msg))) Nothing
            _ -> undefined

data A = A1 | A2
data SA :: A -> Type where
  SA1 :: SA A1
  SA2 :: SA A2

-- p :: exists x. SA x
-- p = SA1

p :: SA x
p = unsafeCoerce SA1

-- https://github.com/goldfirere/ghc-proposals/blob/existentials/proposals/0473-existentials.rst
-- SS2 :: exists. SBookSt (S2 (b :: Bool))
--                                         S2 s
-- Afford :: Msg Role BookSt Buyer Seller (S2 True) '(S3, S3)

data CheckPriceResult :: BookSt -> Type where
  CheckTrue :: CheckPriceResult (S2 True)
  CheckFalse :: CheckPriceResult (S2 False)

checkPrice :: Int -> Peer Role BookSt Buyer IO CheckPriceResult S12
checkPrice i =
  if i < 100
    then LiftM $ pure (ireturn CheckTrue)
    else LiftM $ pure (ireturn CheckFalse)

buyerPeer
  :: Peer Role BookSt Buyer IO (At (Maybe Date) (Done Buyer)) S0
buyerPeer = I.do
  yield (Title "haskell book")
  liftm $ putStrLn "buyer send: haskell book"
  Recv (Price i) <- await
  liftm $ putStrLn "buyer recv: price"
  res <- checkPrice i
  case res of
    CheckTrue -> I.do
      yield Afford
      liftm $ putStrLn "buyer can buy, send Afford"
      Recv (Date d) <- await
      liftm $ putStrLn "buyer recv: Date, Finish"
      returnAt (Just d)
    CheckFalse -> I.do
      yield NotBuy
      liftm $ putStrLn "buyer can't buy, send NotBuy, Finish"
      returnAt Nothing

sellerPeer :: Peer Role BookSt Seller IO (At () (Done Seller)) S0
sellerPeer = I.do
  Recv (Title _name) <- await
  liftm $ putStrLn "seller recv: Title"
  yield (Price 30)
  liftm $ putStrLn "seller send: Price"
  Recv msg <- await
  case msg of
    Afford -> I.do
      liftm $ putStrLn "seller recv: Afford"
      yield (Date 100)
      liftm $ putStrLn "seller send: Date, Finish"
    NotBuy -> I.do
      liftm $ putStrLn "seller recv: NotBuy, Finish"
      returnAt ()

newTMV :: s -> IO (s, TMVar a)
newTMV s = do
  ntmv <- newEmptyTMVarIO
  pure (s, ntmv)

runAll :: IO ()
runAll = do
  buyerR <- newTMV Buyer
  selleR <- newTMV Seller

  let channel1 = mvarsAsChannel selleR [buyerR]
      channel2 = mvarsAsChannel buyerR [selleR]

  forkIO $ void $ do
    runPeerWithDriver (driverSimple codecRoleBookSt channel1) sellerPeer Nothing

  runPeerWithDriver (driverSimple codecRoleBookSt channel2) buyerPeer Nothing
  pure ()
