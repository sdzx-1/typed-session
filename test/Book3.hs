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

module Book3 where

import Control.Concurrent.Class.MonadSTM
import Control.Monad
import Control.Monad.Class.MonadFork (MonadFork, forkIO)
import Control.Monad.Class.MonadSay
import Control.Monad.Class.MonadThrow (MonadThrow)
import Control.Monad.IO.Class (liftIO)
import Data.IFunctor (At (..), Sing, SingI, ireturn, returnAt)
import qualified Data.IFunctor as I
import qualified Data.IntMap as IntMap
import Data.Kind
import GHC.Exts (dataToTag#)
import GHC.Int (Int (I#))
import System.Random (randomIO)
import TypedProtocol.Codec
import TypedProtocol.Core
import TypedProtocol.Driver

{-
---------------------------Buyer-------------------------Seller------------------------Buyer2---------------------------
                             S0                            S0                          (S1 s)
            Title            |            ----->           |
                           (S2 s)                        (S2 s)                        (S1 s)
    ---------------------------------------------------[NotFound]---------------------------------------------------
                           (S2 s)                   (S2 [NotFound])                    (S1 s)
            NoBook           |            <-----           |
                      (S1 [NotFound])                     End                          (S1 s)
         SellerNoBook        |                           ----->                          |
                            End                           End                           End
                                                        Terminal

    ----------------------------------------------------[Found]-----------------------------------------------------
                           (S2 s)                     (S2 [Found])                     (S1 s)
            Price            |            <-----           |
                           (S1 s)                        (S3 s)                        (S1 s)
        ----------------------------------------------[One,Found]-----------------------------------------------
                      (S1 [One,Found])                   (S3 s)                        (S1 s)
          OneAfford          |                           ----->                          |
                      (S3 [One,Found])                   (S3 s)                          S4
          OneAccept          |            ----->           |
                             S5                            S5                            S4
           OneDate           |            <-----           |
                             S4                           End                            S4
          OneSuccess         |                           ----->                          |
                            End                           End                           End
                                                        Terminal

        ----------------------------------------------[Two,Found]-----------------------------------------------
                      (S1 [Two,Found])                   (S3 s)                        (S1 s)
        PriceToBuyer2        |                           ----->                          |
                           (S6 s)                        (S3 s)                        (S6 s)
            -------------------------------------[NotSupport,Two,Found]-------------------------------------
                           (S6 s)                        (S3 s)             (S6 [NotSupport,Two,Found])
          NotSupport1        |                           <-----                          |
                (S3 [NotSupport,Two,Found])              (S3 s)                         End
          TwoNotBuy          |            ----->           |
                            End                           End                           End
                                                        Terminal

            --------------------------------------[Support,Two,Found]---------------------------------------
                           (S6 s)                        (S3 s)               (S6 [Support,Two,Found])
          SupportVal         |                           <-----                          |
                           (S3 s)                        (S3 s)                        (S7 s)
                -------------------------------[Enough,Support,Two,Found]-------------------------------
              (S3 [Enough,Support,Two,Found])            (S3 s)                        (S7 s)
          TwoAccept          |            ----->           |
                             S8                            S8                          (S7 s)
           TwoDate           |            <-----           |
              (S7 [Enough,Support,Two,Found])             End                          (S7 s)
          TwoSuccess         |                           ----->                          |
                            End                           End                           End
                                                        Terminal

                -----------------------------[NotEnough,Support,Two,Found]------------------------------
             (S3 [NotEnough,Support,Two,Found])          (S3 s)                        (S7 s)
          TwoNotBuy1         |            ----->           |
             (S7 [NotEnough,Support,Two,Found])           End                          (S7 s)
          TwoFailed          |                           ----->                          |
                            End                           End                           End
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

data BookBranchSt
  = NotFound
  | Found
  | One
  | Two
  | Support
  | NotSupport
  | Enough
  | NotEnough
  deriving (Show)

data BookSt
  = S0
  | S1 [BookBranchSt]
  | S2 [BookBranchSt]
  | S3 [BookBranchSt]
  | S4
  | S5
  | S6 [BookBranchSt]
  | S7 [BookBranchSt]
  | S8
  | End

data SBookSt :: BookSt -> Type where
  SS0 :: SBookSt S0
  SS1 :: SBookSt (S1 s)
  SS2 :: SBookSt (S2 s)
  SS3 :: SBookSt (S3 s)
  SS4 :: SBookSt S4
  SS5 :: SBookSt S5
  SS6 :: SBookSt (S6 s)
  SS7 :: SBookSt (S7 s)
  SS8 :: SBookSt S8
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

instance SingI S5 where
  sing = SS5

instance SingI (S6 s) where
  sing = SS6

instance SingI (S7 s) where
  sing = SS7

instance SingI S8 where
  sing = SS8

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
    NoBook :: Msg Role BookSt (S2 '[NotFound]) '(Seller, End) '(Buyer, S1 '[NotFound])
    SellerNoBook :: Msg Role BookSt (S1 '[NotFound]) '(Buyer, End) '(Buyer2, End)
    Price :: Int -> Msg Role BookSt (S2 '[Found]) '(Seller, S3 s) '(Buyer, S1 s)
    OneAfford :: Msg Role BookSt (S1 '[One, Found]) '(Buyer, S3 '[One, Found]) '(Buyer2, S4)
    OneAccept :: Msg Role BookSt (S3 '[One, Found]) '(Buyer, S5) '(Seller, S5)
    OneDate :: Date -> Msg Role BookSt S5 '(Seller, End) '(Buyer, S4)
    OneSuccess :: Date -> Msg Role BookSt S4 '(Buyer, End) '(Buyer2, End)
    PriceToBuyer2 :: Int -> Msg Role BookSt (S1 '[Two, Found]) '(Buyer, S6 s) '(Buyer2, S6 s)
    NotSupport1 :: Msg Role BookSt (S6 '[NotSupport, Two, Found]) '(Buyer2, End) '(Buyer, S3 '[NotSupport, Two, Found])
    TwoNotBuy :: Msg Role BookSt (S3 '[NotSupport, Two, Found]) '(Buyer, End) '(Seller, End)
    SupportVal :: Int -> Msg Role BookSt (S6 '[Support, Two, Found]) '(Buyer2, S7 s) '(Buyer, S3 s)
    TwoAccept :: Msg Role BookSt (S3 '[Enough, Support, Two, Found]) '(Buyer, S8) '(Seller, S8)
    TwoDate :: Date -> Msg Role BookSt S8 '(Seller, End) '(Buyer, S7 '[Enough, Support, Two, Found])
    TwoSuccess :: Date -> Msg Role BookSt (S7 '[Enough, Support, Two, Found]) '(Buyer, End) '(Buyer2, End)
    TwoNotBuy1 :: Msg Role BookSt (S3 '[NotEnough, Support, Two, Found]) '(Buyer, S7 '[NotEnough, Support, Two, Found]) '(Seller, End)
    TwoFailed :: Msg Role BookSt (S7 '[NotEnough, Support, Two, Found]) '(Buyer, End) '(Buyer2, End)

encodeMsg :: Encode Role BookSt (AnyMsg Role BookSt)
encodeMsg = Encode $ \x -> case x of
  Title{} -> AnyMsg x
  NoBook{} -> AnyMsg x
  SellerNoBook{} -> AnyMsg x
  Price{} -> AnyMsg x
  OneAfford{} -> AnyMsg x
  OneAccept{} -> AnyMsg x
  OneDate{} -> AnyMsg x
  OneSuccess{} -> AnyMsg x
  PriceToBuyer2{} -> AnyMsg x
  NotSupport1{} -> AnyMsg x
  TwoNotBuy{} -> AnyMsg x
  SupportVal{} -> AnyMsg x
  TwoAccept{} -> AnyMsg x
  TwoDate{} -> AnyMsg x
  TwoSuccess{} -> AnyMsg x
  TwoNotBuy1{} -> AnyMsg x
  TwoFailed{} -> AnyMsg x

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
  Yes :: CheckPriceResult (S3 [Enough, Support, Two, Found])
  No :: CheckPriceResult (S3 [NotEnough, Support, Two, Found])

checkPrice :: Int -> Int -> Peer Role BookSt Buyer IO CheckPriceResult (S3 s)
checkPrice _i _h = I.do
  At b <- liftm $ liftIO $ randomIO @Bool
  if b
    then LiftM $ pure (ireturn Yes)
    else LiftM $ pure (ireturn No)

data OT :: BookSt -> Type where
  OTOne :: OT (S1 [One, Found])
  OTTwo :: OT (S1 [Two, Found])

choiceOT :: Int -> Peer Role BookSt Buyer IO OT (S1 s)
choiceOT _i = I.do 
  At b <- liftm $ liftIO $ randomIO @Bool
  if b
    then LiftM $ pure $ ireturn OTOne
    else LiftM $ pure $ ireturn OTTwo

buyerPeer
  :: Peer Role BookSt Buyer IO (At (Maybe Date) (Done Buyer)) S0
buyerPeer = I.do
  yield (Title "haskell book")
  await I.>>= \case
    Recv NoBook -> I.do
      yield SellerNoBook
      returnAt Nothing
    Recv (Price i) -> I.do
      choiceOT i I.>>= \case
        OTOne -> I.do
          yield OneAfford
          yield OneAccept
          Recv (OneDate d) <- await
          yield (OneSuccess d)
          returnAt $ Just d
        OTTwo -> I.do
          yield (PriceToBuyer2 i)
          await I.>>= \case
            Recv NotSupport1 -> I.do
              yield TwoNotBuy
              returnAt Nothing
            Recv (SupportVal h) -> I.do
              checkPrice i h I.>>= \case
                Yes -> I.do
                  yield TwoAccept
                  Recv (TwoDate d) <- await
                  yield (TwoSuccess d)
                  returnAt (Just d)
                No -> I.do
                  yield TwoNotBuy1
                  yield TwoFailed
                  returnAt Nothing

data BuySupp :: BookSt -> Type where
  BNS :: BuySupp (S6 '[NotSupport, Two, Found])
  BS :: BuySupp (S6 '[Support, Two, Found])

choiceB :: Int -> Peer Role BookSt Buyer2 IO BuySupp (S6 s)
choiceB _i = I.do 
  At b <- liftm $ liftIO $ randomIO @Bool
  if b
    then LiftM $ pure $ ireturn BNS
    else LiftM $ pure $ ireturn BS

buyer2Peer
  :: Peer Role BookSt Buyer2 IO (At (Maybe Date) (Done Buyer2)) (S1 s)
buyer2Peer = I.do
  await I.>>= \case
    Recv SellerNoBook -> returnAt Nothing
    Recv OneAfford -> I.do
      Recv (OneSuccess d) <- await
      returnAt (Just d)
    Recv (PriceToBuyer2 i) -> I.do
      choiceB i I.>>= \case
        BNS -> I.do
          yield NotSupport1
          returnAt Nothing
        BS -> I.do
          yield (SupportVal (i `div` 2))
          await I.>>= \case
            Recv (TwoSuccess d) -> returnAt $ Just d
            Recv TwoFailed -> returnAt Nothing

data FindBookResult :: BookSt -> Type where
  NotFound' :: FindBookResult (S2 '[NotFound])
  Found' :: FindBookResult (S2 '[Found])

findBook :: String -> Peer Role BookSt Seller IO FindBookResult (S2 s)
findBook _st = I.do 
  At b <- liftm $ liftIO $ randomIO @Bool
  if b
    then LiftM $ pure (ireturn Found')
    else LiftM $ pure (ireturn NotFound')

sellerPeer :: Peer Role BookSt Seller IO (At () (Done Seller)) S0
sellerPeer = I.do
  Recv (Title st) <- await
  findBook st I.>>= \case
    NotFound' -> yield NoBook
    Found' -> I.do
      yield (Price 30)
      await I.>>= \case
        Recv OneAccept -> yield (OneDate 100)
        Recv TwoNotBuy -> returnAt ()
        Recv TwoAccept -> yield (TwoDate 100)
        Recv TwoNotBuy1 -> returnAt ()

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
    NoBook -> "NoBook"
    SellerNoBook -> "SellerNoBook"
    Price i -> "Price " <> show i
    OneAfford -> "OneAfford"
    OneAccept -> "OneAccept"
    OneDate d -> "OneDate " <> show d
    OneSuccess d -> "OneSuccess" <> show d
    PriceToBuyer2 i -> "PriceToBuyer2 " <> show i
    NotSupport1 -> "NotSupport1"
    TwoNotBuy -> "TwoNotBuy"
    SupportVal v -> "SupportVal " <> show v
    TwoAccept -> "TwoAccept"
    TwoDate d -> "TwoDate " <> show d
    TwoSuccess d -> "TwoSuccess " <> show d
    TwoNotBuy1 -> "TwoNotBuy1"
    TwoFailed -> "TwoFailed"

runAll :: IO ()
runAll = do
  buyerTMVar <- newEmptyTMVarIO @_ @(AnyMsg Role BookSt)
  buyer2TMVar <- newEmptyTMVarIO @_ @(AnyMsg Role BookSt)
  sellerTMVar <- newEmptyTMVarIO @_ @(AnyMsg Role BookSt)
  let buyerSellerChannel = mvarsAsChannel @_ buyerTMVar sellerTMVar
      buyerBuyer2Channel = mvarsAsChannel @_ buyerTMVar buyer2TMVar

      sellerBuyerChannel = mvarsAsChannel @_ sellerTMVar buyerTMVar

      buyer2BuyerChannel = mvarsAsChannel @_ buyer2TMVar buyerTMVar

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
  let buyerDriver = driverSimple (myTracer "buyer :") encodeMsg sendToRole buyerTvar
      buyer2Driver = driverSimple (myTracer "buyer2 :") encodeMsg sendToRole buyer2Tvar
      sellerDriver = driverSimple (myTracer "seller :") encodeMsg sendToRole sellerTvar
  -- fork buyer decode thread, seller -> buyer
  forkIO $ decodeLoop (myTracer "buyer :") Nothing (Decode decodeMsg) buyerSellerChannel buyerTvar
  -- fork buyer decode thread, buyer2 -> buyer
  forkIO $ decodeLoop (myTracer "buyer :") Nothing (Decode decodeMsg) buyerBuyer2Channel buyerTvar

  -- fork seller decode thread, buyer -> seller
  forkIO $ decodeLoop (myTracer "seller :") Nothing (Decode decodeMsg) sellerBuyerChannel sellerTvar

  -- fork buyer2 decode thread, buyer -> buyer2
  forkIO $ decodeLoop (myTracer "buyer2 :") Nothing (Decode decodeMsg) buyer2BuyerChannel buyer2Tvar

  -- fork seller Peer thread
  forkIO $ void $ runPeerWithDriver sellerDriver sellerPeer

  -- fork buyer2 Peer thread
  forkIO $ void $ runPeerWithDriver buyer2Driver buyer2Peer
  -- run buyer Peer
  void $ runPeerWithDriver buyerDriver buyerPeer
