# typed-communication-protocol
Typed communication protocol allows communication between any number of roles.

The typed-communication-protocol uses [Mcbride Indexed Monad](https://stackoverflow.com/questions/28690448/what-is-indexed-monad).

This library requires the latest ghc 9.10.1！

Here is a examples：The-Bug-ProneBookseller


The example comes from [HasChor: Functional Choreographic Programming for All](https://dl.acm.org/doi/10.1145/3607849)


Detailed description:



<img width="592" alt="屏幕截图 2024-04-22 230819" src="https://github.com/sdzx-1/typed-communication-protocol/assets/63328943/0b8b129e-74bf-445e-a1c6-96d4f5c79f47">

```haskell
{-

--------------------------------------------------------------------------
    Buyer                                                      Seller
    :S0                                                        :S0
     <                     Title String  ->                     >
    :S1                                                        :S1
     <                     <-  Price Int                         >
    :S12                                                       :S12

   ---------------------------------------------------------------------
   |:S12                                                       :S12
   | <                  Afford ->                               >
   |:S3                                                        :S3
   | <                  <- Data Int                             >
   |:End                                                       :End
   ---------------------------------------------------------------------

   ---------------------------------------------------------------------
   |:S12                                                       :S12
   | <                  NotBuy ->                               >
   |:End                                                       :End
   ---------------------------------------------------------------------

-}

```

Two-buyer bookseller protocol



<img width="597" alt="屏幕截图 2024-04-22 231527" src="https://github.com/sdzx-1/typed-communication-protocol/assets/63328943/c7f11579-74fd-481e-bf70-140caddfa25e">

```haskell
{-

-----------------------------------------------------------------------------------------------
    Buyer                                                      Seller                  Buyer2
    :S0                                                        :S0
     <                     Title String  ->                     >
    :S1                                                        :S1
     <                     <-  Price Int                         >
    :S11                                                       :S12                    :S11
     <                                  PriceToBuyer2 Int ->                            >
    :S110                                                                              :S110
     <                                  <- HalfPrice  Int                               >
    :S12                                                                               :End

   ---------------------------------------------------------------------
   |:S12                                                       :S12
   | <                  Afford ->                               >
   |:S3                                                        :S3
   | <                  <- Data Int                             >
   |:End                                                       :End
   ---------------------------------------------------------------------

   ---------------------------------------------------------------------
   |:S12                                                       :S12
   | <                  NotBuy ->                               >
   |:End                                                       :End
   ---------------------------------------------------------------------


-}
```
[Code:](https://github.com/sdzx-1/typed-communication-protocol/blob/main/test/Book1.hs)
```haskell

data Role = Buyer | Seller | Buyer2
  deriving (Show, Eq, Ord)

data BookSt
  = S0
  | S1
  | S11
  | S110
  | S12
  | S3
  | End


type Date = Int

instance Protocol Role BookSt where
  type Done Buyer = End
  type Done Seller = End
  type Done Buyer2 = End
  data Msg Role BookSt send recv from to where
    Title :: String -> Msg Role BookSt Buyer Seller S0 '(S1, S1)
    Price :: Int -> Msg Role BookSt Seller Buyer S1 '(S12, S11)
    PriceToB2 :: Int -> Msg Role BookSt Buyer Buyer2 S11 '(S110, S110)
    HalfPrice :: Int -> Msg Role BookSt Buyer2 Buyer S110 '(End, S12)
    Afford :: Msg Role BookSt Buyer Seller S12 '(S3, S3)
    Date :: Date -> Msg Role BookSt Seller Buyer S3 '(End, End)
    NotBuy :: Msg Role BookSt Buyer Seller S12 '(End, End)

budget :: Int
budget = 16

buyerPeer
  :: Peer Role BookSt Buyer IO (At (Maybe Date) (Done Buyer)) S0
buyerPeer = I.do
  yield (Title "haskell book")
  Recv (Price i) <- await
  yield (PriceToB2 i)
  Recv (HalfPrice hv) <- await
  if i <= hv + budget
    then I.do
      yield Afford
      Recv (Date d) <- await
      returnAt (Just d)
    else I.do
      yield NotBuy
      returnAt Nothing

buyerPeer2
  :: Peer Role BookSt Buyer2 IO (At () (Done Buyer2)) S11
buyerPeer2 = I.do
  Recv (PriceToB2 i) <- await
  yield (HalfPrice (i `div` 2))

sellerPeer :: Peer Role BookSt Seller IO (At () (Done Seller)) S0
sellerPeer = I.do
  Recv (Title _name) <- await
  yield (Price 30)
  Recv msg <- await
  case msg of
    Afford -> yield (Date 100)
    NotBuy -> returnAt ()
```
---------------------------------------------
You can run these two examples: cabal test

result: 

```shell
----------------- run Book -----------------
Buyer: Send Title "haskell book"
Seller: Recv Title "haskell book"
Seller: Send Price 30
Buyer: Recv Price 30
Buyer: Send Afford
Seller: Recv Afford
Seller: Send Date 100
Buyer: Recv Date 100
---------------- run Book1 -----------------
Buyer: Send Title "haskell book"
Seller: Recv Title "haskell book"
Seller: Send Price 30
Buyer: Recv Price 30
Buyer: Send PriceToB2 30
Buyer2: Recv PriceToB2 30
Buyer2: Send HalfPrice 15
Buyer: Recv HalfPrice 15
Buyer: Send Afford
Seller: Recv Afford
Seller: Send Date 100
Buyer: Recv Date 100
--------------------------------------------

```


