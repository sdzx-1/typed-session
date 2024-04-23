{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module TypedProtocol.Codec where

import Control.Concurrent.STM
import Control.Exception (Exception)
import Data.Map (Map)
import qualified Data.Map as Map
import TypedProtocol.Core

data Codec role' ps failure m bytes = Codec
  { encode
      :: forall (send :: role') (recv :: role') (st :: ps) (st' :: ps) (st'' :: ps)
       . Agency role' ps recv st
      -> Msg role' ps send recv st '(st', st'')
      -> bytes
  , decode
      :: forall (recv :: role') (from :: ps)
       . Agency role' ps recv from
      -> m (DecodeStep bytes failure m (SomeMsg role' ps recv from))
  }

data DecodeStep bytes failure m a
  = DecodePartial (Maybe bytes -> m (DecodeStep bytes failure m a))
  | DecodeDone a (Maybe bytes)
  | DecodeFail failure

data CodecFailure
  = CodecFailureOutOfInput
  | CodecFailure String
  deriving (Eq, Show)

instance Exception CodecFailure

runDecoder
  :: (Monad m)
  => [bytes]
  -> DecodeStep bytes failure m a
  -> m (Either failure a)
runDecoder _ (DecodeDone x _trailing) = return (Right x)
runDecoder _ (DecodeFail failure) = return (Left failure)
runDecoder [] (DecodePartial k) = k Nothing >>= runDecoder []
runDecoder (b : bs) (DecodePartial k) = k (Just b) >>= runDecoder bs

data AnyMessage role' ps where
  AnyMessage
    :: Msg role' ps send recv st '(st', st'')
    -> AnyMessage role' ps

data Channel role' m a = Channel
  { sendMap :: Map role' (a -> m ())
  , recv :: m (Maybe a)
  }

mvarsAsChannel
  :: (Ord role')
  => (role', TMVar a)
  -> [(role', TMVar a)]
  -> Channel role' IO a
mvarsAsChannel (_, bufferRead) bufferWrites =
  Channel{sendMap, recv}
 where
  sendMap = Map.fromList $ map (\(i, tmvar) -> (i, atomically . putTMVar tmvar)) bufferWrites
  recv = atomically (Just <$> takeTMVar bufferRead)

runDecoderWithChannel
  :: (Monad m)
  => Channel role' m bytes
  -> Maybe bytes
  -> DecodeStep bytes failure m a
  -> m (Either failure (a, Maybe bytes))
runDecoderWithChannel Channel{recv} = go
 where
  go _ (DecodeDone x trailing) = return (Right (x, trailing))
  go _ (DecodeFail failure) = return (Left failure)
  go Nothing (DecodePartial k) = recv >>= k >>= go Nothing
  go (Just trailing) (DecodePartial k) = k (Just trailing) >>= go Nothing
