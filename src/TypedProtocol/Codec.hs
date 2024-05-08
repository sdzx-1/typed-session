--  This part of the code comes from typed-protocols, I modified a few things.
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module TypedProtocol.Codec where

import Control.Exception (Exception)
import Data.Dependent.Map (DMap)
import Data.IFunctor (Any, Sing)
import TypedProtocol.Core

newtype Encode role' ps bytes = Encode
  { encode
      :: forall (send :: role') (recv :: role') (st :: ps) (st' :: ps) (st'' :: ps)
       . Msg role' ps st '(send, st') '(recv, st'')
      -> bytes
  }

newtype Decode role' ps failure m bytes = Decode
  { decode :: DecodeStep bytes failure m (AnyMsg role' ps)
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

data Channel m a = Channel
  { send :: a -> m ()
  , recv :: m (Maybe a)
  }

type SendToRole role' m a = DMap (Sing @role') (Any (a -> m ()))

runDecoderWithChannel
  :: (Monad m)
  => Channel m bytes
  -> Maybe bytes
  -> DecodeStep bytes failure m a
  -> m (Either failure (a, Maybe bytes))
runDecoderWithChannel Channel{recv} = go
 where
  go _ (DecodeDone x trailing) = return (Right (x, trailing))
  go _ (DecodeFail failure) = return (Left failure)
  go Nothing (DecodePartial k) = recv >>= k >>= go Nothing
  go (Just trailing) (DecodePartial k) = k (Just trailing) >>= go Nothing
