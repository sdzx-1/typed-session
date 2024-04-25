--  This part of the code comes from typed-protocols, I modified a few things.
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

import Control.Exception (Exception)
import Data.SR
import TypedProtocol.Core

data Codec role' ps failure m bytes = Codec
  { encode
      :: forall (send :: role') (recv :: role') (st :: ps) (st' :: ps) (st'' :: ps)
       . Agency role' ps recv st
      -> Msg role' ps st '(send, st') '(recv, st'')
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

data Channel role' m a = Channel
  { sendFun :: forall r. Sing (r :: role') -> a -> m ()
  , recv :: m (Maybe a)
  }

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
