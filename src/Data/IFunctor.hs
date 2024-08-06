{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Data.IFunctor where

import Data.Data
import Data.Kind

{- |
Singletons are not used here. I am not sure if singletons can generate the instances I need.

Here is an example: Ping-Pong

@
data PingPong
  = S0 [Bool]
  | S1
  | S2 [Bool]
  | End

data SPingPong :: PingPong -> Type where
  SS0 :: SPingPong (S0 b)
  SS1 :: SPingPong S1
  SS2 :: SPingPong (S2 b)
  SEnd :: SPingPong End
@

Note here /SS0 :: SPingPong (S0 b)/

Using singletons will generate /SS0 :: Sing b -> SPingPong (S0 b)/ which is not what I need.

Please note the following example:

@
serverPeer ::
  (Monad m) => Peer Role PingPong Server m (At () (Done Server)) (S0 s)
serverPeer = I.do
  -- The server is in a state (S0 s) while it is awaiting a message,
  -- and its state is indeterminate until it receives a message.
  -- SS0 :: SPingPong (S0 b) correctly indicates this indeterminacy.
  Recv msg <- await
  case msg of
    Ping -> I.do
      yield Pong
      serverPeer
    Stop -> returnAt ()
@
-}
type family Sing :: k -> Type

type SingI :: forall {k}. k -> Constraint

{- |

example：Ping-Pong

@
type instance Sing = SPingPong

instance SingI (S0 b) where
  sing = SS0

instance SingI S1 where
  sing = SS1

instance SingI (S2 b) where
  sing = SS2

instance SingI End where
  sing = SEnd
@
-}
class SingI a where
  sing :: Sing a

infixr 0 ~>

type f ~> g = forall x. f x -> g x

class IFunctor f where
  imap :: (a ~> b) -> f a ~> f b

{- | McBride Indexed Monads

Here's Edward Kmett's [introduction to Indexed Monads](https://stackoverflow.com/questions/28690448/what-is-indexed-monad).

As he said, there are at least three indexed monads:


* Bob Atkey

@
class IMonad m where
  ireturn  ::  a -> m i i a
  ibind    ::  m i j a -> (a -> m j k b) -> m i k b
@

* Conor McBride

@
type a ~> b = forall i. a i -> b i

class IMonad m where
  ireturn :: a ~> m a
  ibind :: (a ~> m b) -> (m a ~> m b)
@

* Dominic Orchard

No detailed description, just a link to this [lecture](https://github.com/dorchard/effect-monad/blob/master/docs/ixmonad-fita14.pdf)。

I use  the McBride Indexed Monad, the earliest paper [here](https://personal.cis.strath.ac.uk/conor.mcbride/Kleisli.pdf).

The following is my understanding of (\~>): through GADT, let the value contain type information,
and then use ((\~>), pattern match) to pass the type to subsequent functions

@
data V = A | B

data SV :: V -> Type where  -- GADT, let the value contain type information
   SA :: SV A
   SB :: SV B

data SV1 :: V -> Type where
   SA1 :: SV1 A
   SB1 :: SV1 B

fun :: SV ~> SV1     -- type f ~> g = forall x. f x -> g x
fun sv = case sv of  -- x is arbitrary but f, g must have the same x
     SA -> SA1       -- Pass concrete type state to subsequent functions via pattern matching
     SB -> SB1


class (IFunctor m) => IMonad m where
  ireturn :: a ~> m a
  ibind :: (a ~> m b)  -- The type information contained in a will be passed to (m b),
                       -- which is exactly what we need: external input has an impact on the type!
        -> m a ~> m b
@
-}
class (IFunctor m) => IMonad m where
  ireturn :: a ~> m a
  ibind :: (a ~> m b) -> m a ~> m b

class (IMonad m) => IMonadFail m where
  fail :: String -> m a ix

data At :: Type -> k -> k -> Type where
  At :: a -> At a k k
  deriving (Typeable)

(>>=) :: (IMonad (m :: (x -> Type) -> x -> Type)) => m a ix -> (a ~> m b) -> m b ix
m >>= f = ibind f m

(>>) :: (IMonad (m :: (x -> Type) -> x -> Type)) => m (At a j) i -> m b j -> m b i
m >> f = ibind (\(At _) -> f) m

returnAt :: (IMonad m) => a -> m (At a k) k
returnAt = ireturn . At
