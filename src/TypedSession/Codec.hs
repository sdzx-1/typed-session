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

module TypedSession.Codec where

import Control.Exception (Exception)
import TypedSession.Core

{- |
Function to encode Msg into bytes.
-}
newtype Encode role' ps bytes = Encode
  { encode
      :: forall (send :: role') (recv :: role') (st :: ps) (st' :: ps) (st'' :: ps)
       . Msg role' ps st send st' recv st''
      -> bytes
  }

{- |
Incremental decoding function.
-}
newtype Decode role' ps failure bytes = Decode
  { decode :: DecodeStep bytes failure (AnyMsg role' ps)
  }

{- |
Generic incremental decoder constructor, you need to convert specific incremental decoders to it.
-}
data DecodeStep bytes failure a
  = DecodePartial (Maybe bytes -> (DecodeStep bytes failure a))
  | DecodeDone a (Maybe bytes)
  | DecodeFail failure

data CodecFailure
  = CodecFailureOutOfInput
  | CodecFailure String
  deriving (Eq, Show)

instance Exception CodecFailure

{- |
Bottom functions for sending and receiving bytes.
-}
data Channel m bytes = Channel
  { send :: bytes -> m ()
  , recv :: m (Maybe bytes)
  }

{- |
Generic incremental decoding function.
-}
runDecoderWithChannel
  :: (Monad m)
  => Channel m bytes
  -> Maybe bytes
  -> DecodeStep bytes failure a
  -> m (Either failure (a, Maybe bytes))
runDecoderWithChannel Channel{recv} = go
 where
  go _ (DecodeDone x trailing) = return (Right (x, trailing))
  go _ (DecodeFail failure) = return (Left failure)
  go Nothing (DecodePartial k) = recv >>= pure . k >>= go Nothing
  go (Just trailing) (DecodePartial k) = (pure . k) (Just trailing) >>= go Nothing
