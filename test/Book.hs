{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE QualifiedDo #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}

module Book where

import Data.IFunctor (At (..), ireturn, returnAt)
import qualified Data.IFunctor as I
import Data.Kind
import TypedProtocol.Core

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

data BookSt
  = S0
  | S1
  | S12
  | S2 Bool
  | S3
  | End

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
  Recv (Price i) <- await
  res <- checkPrice i
  case res of
    CheckTrue -> I.do
      yield Afford
      Recv (Date d) <- await
      returnAt (Just d)
    CheckFalse -> I.do
      yield NotBuy
      returnAt Nothing

sellerPeer :: Peer Role BookSt Seller IO (At () (Done Seller)) S0
sellerPeer = I.do
  Recv (Title _name) <- await
  yield (Price 30)
  Recv msg <- await
  case msg of
    Afford -> yield (Date 100)
    NotBuy -> returnAt ()
