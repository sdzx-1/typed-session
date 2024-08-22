{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE QualifiedDo #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# OPTIONS_GHC -Wall #-}

module Book3.Codec where

import TypedSession.Codec
import TypedSession.Core
import qualified Data.ByteString.Lazy as L
import Data.Binary
import Data.Binary.Put
import Data.Binary.Get
import qualified Network.Socket as Socket
import qualified Network.Socket.ByteString as Socket
import qualified Data.ByteString.Builder.Extra as L
import qualified Data.ByteString as BS
import Book3.Type
import Book3.Protocol
import TypedSession.Driver
import Control.Monad.Class.MonadSay (MonadSay (say))

encodeMsg :: Encode BookRole Book L.ByteString
encodeMsg  = Encode $ \x -> runPut $  case x of
  Title st ->        putWord8 0 >> put st
  NoBook ->          putWord8 1
  SellerNoBook ->    putWord8 2
  Price i ->         putWord8 3 >> put i
  OneAccept ->       putWord8 5 
  OneDate i ->       putWord8 6 >> put i
  OneSuccess i ->    putWord8 7 >> put i
  PriceToBuyer2 i -> putWord8 8 >> put i
  NotSupport1 ->     putWord8 9
  TwoNotBuy ->       putWord8 10
  SupportVal i ->    putWord8 11 >> put i
  TwoAccept ->       putWord8 12
  TwoDate i ->       putWord8 13 >> put i
  TwoSuccess i ->    putWord8 14 >> put i
  TwoNotBuy1 ->      putWord8 15
  TwoFailed ->       putWord8 16


getAnyMsg :: Get (AnyMsg BookRole Book)
getAnyMsg = do 
  v <- getWord8
  case v of
  -- Title st ->        putWord8 0 >> put st
    0 -> get >>= pure . AnyMsg . Title
  -- NoBook ->          putWord8 1
    1 -> pure (AnyMsg  NoBook)
  -- SellerNoBook ->    putWord8 2
    2 -> pure (AnyMsg  SellerNoBook)
  -- Price i ->         putWord8 3 >> put i
    3 -> get >>= pure . AnyMsg . Price
  -- OneAccept ->       putWord8 5 
    5 -> pure (AnyMsg OneAccept)
  -- OneDate i ->       putWord8 6 >> put i
    6 -> get >>= pure . AnyMsg . OneDate
  -- OneSuccess i ->    putWord8 7 >> put i
    7 -> get >>= pure . AnyMsg . OneSuccess
  -- PriceToBuyer2 i -> putWord8 8 >> put i
    8 -> get >>= pure . AnyMsg . PriceToBuyer2
  -- NotSupport1 ->     putWord8 9
    9 -> pure (AnyMsg NotSupport1)
  -- TwoNotBuy ->       putWord8 10
    10 -> pure (AnyMsg TwoNotBuy)
  -- SupportVal i ->    putWord8 11 >> put i
    11 -> get >>= pure . AnyMsg . SupportVal
  -- TwoAccept ->       putWord8 12
    12 -> pure (AnyMsg TwoAccept)
  -- TwoDate i ->       putWord8 13 >> put i
    13 -> get >>= pure . AnyMsg . TwoDate
  -- TwoSuccess i ->    putWord8 14 >> put i
    14 -> get >>= pure . AnyMsg . TwoSuccess
  -- TwoNotBuy1 ->      putWord8 15
    15 -> pure (AnyMsg TwoNotBuy1)
  -- TwoFailed ->       putWord8 16
    16 -> pure (AnyMsg TwoFailed)
    i -> error $ "undefined index: " ++ show i

convertDecoderLBS1 :: Decoder a 
                   ->  (DecodeStep L.ByteString CodecFailure  a)
convertDecoderLBS1 = go 
  where 
    go :: Decoder a -> DecodeStep L.ByteString CodecFailure  a
    go (Done tr _ a ) = DecodeDone a (Just $ L.fromStrict tr)
    go (Fail _ _ e) = DecodeFail (CodecFailure e)
    go (Partial k) = DecodePartial $ \mbs -> case mbs of 
                                   Nothing -> go (Partial k) 
                                   Just _bs ->  go (k $ fmap L.toStrict mbs)

decodeMsg
  :: DecodeStep
      L.ByteString
      CodecFailure
      (AnyMsg BookRole Book)
decodeMsg = convertDecoderLBS1 (runGetIncremental getAnyMsg)

socketAsChannel :: Socket.Socket -> Channel IO L.ByteString
socketAsChannel socket =
    Channel{send, recv}
  where
    send :: L.ByteString -> IO ()
    send chunks =
     Socket.sendMany socket (L.toChunks chunks)

    recv :: IO (Maybe L.ByteString)
    recv = do
      chunk <- Socket.recv socket L.smallChunkSize
      if BS.null chunk
        then return Nothing
        else return (Just (L.fromStrict chunk))

myTracer :: (MonadSay m) => String -> Tracer BookRole Book m
myTracer st v = say (st <> show v)