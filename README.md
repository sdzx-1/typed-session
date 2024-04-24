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
[Code:](https://github.com/sdzx-1/typed-communication-protocol/blob/main/test/Book.hs) 
```haskell
data Role = Buyer | Seller
  deriving (Show, Eq, Ord)

data BookSt
  = S0
  | S1
  | S12
  | S3
  | End


type Date = Int

instance Protocol Role BookSt where
  type Done Buyer = End
  type Done Seller = End

  data Msg Role BookSt send recv from to where
    Title :: String -> Msg Role BookSt Buyer Seller S0 '(S1, S1)
    Price :: Int -> Msg Role BookSt Seller Buyer S1 '(S12, S12)
    Afford :: Msg Role BookSt Buyer Seller S12 '(S3, S3)
    Date :: Date -> Msg Role BookSt Seller Buyer S3 '(End, End)
    NotBuy :: Msg Role BookSt Buyer Seller S12 '(End, End)

budget :: Int
budget = 100

buyerPeer
  :: Peer Role BookSt Buyer IO (At (Maybe Date) (Done Buyer)) S0
buyerPeer = I.do
  liftm $ putStrLn "buyer send: haskell book"
  yield (Title "haskell book")
  Recv (Price i) <- await
  liftm $ putStrLn "buyer recv: price"
  if i <= budget
    then I.do
      liftm $ putStrLn "buyer can buy, send Afford"
      yield Afford
      Recv (Date d) <- await
      liftm $ putStrLn "buyer recv: Date, Finish"
      returnAt (Just d)
    else I.do
      liftm $ putStrLn "buyer can't buy, send NotBuy, Finish"
      yield NotBuy
      returnAt Nothing

sellerPeer :: Peer Role BookSt Seller IO (At () (Done Seller)) S0
sellerPeer = I.do
  Recv (Title _name) <- await
  liftm $ putStrLn "seller recv: Title"
  liftm $ putStrLn "seller send: Price"
  yield (Price 30)
  Recv msg <- await
  case msg of
    Afford -> I.do
      liftm $ putStrLn "seller recv: Afford"
      liftm $ putStrLn "seller send: Date, Finish"
      yield (Date 100)
    NotBuy -> I.do
      liftm $ putStrLn "seller recv: NotBuy, Finish"
      returnAt ()
```
-----------------------------------------


 two-buyer bookseller protocol



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
  liftm $ putStrLn "buyer send: haskell book"
  yield (Title "haskell book")
  Recv (Price i) <- await
  liftm $ putStrLn "buyer recv: price"
  liftm $ putStrLn "buyer send price to b2"
  yield (PriceToB2 i)
  Recv (HalfPrice hv) <- await
  liftm $ putStrLn "buyer recv: b2 half price"
  if i <= hv + budget
    then I.do
      liftm $ putStrLn "buyer can buy, send Afford"
      yield Afford
      Recv (Date d) <- await
      liftm $ putStrLn "buyer recv: Date, Finish"
      returnAt (Just d)
    else I.do
      liftm $ putStrLn "buyer can't buy, send NotBuy, Finish"
      yield NotBuy
      returnAt Nothing

buyerPeer2
  :: Peer Role BookSt Buyer2 IO (At () (Done Buyer2)) S11
buyerPeer2 = I.do
  Recv (PriceToB2 i) <- await
  liftm $ putStrLn "buyer2 recv: price"
  liftm $ putStrLn "buyer2 send half price to buyer, Finish"
  yield (HalfPrice (i `div` 2))

sellerPeer :: Peer Role BookSt Seller IO (At () (Done Seller)) S0
sellerPeer = I.do
  Recv (Title _name) <- await
  liftm $ putStrLn "seller recv: Title"
  liftm $ putStrLn "seller send: Price"
  yield (Price 30)
  Recv msg <- await
  case msg of
    Afford -> I.do
      liftm $ putStrLn "seller recv: Afford"
      liftm $ putStrLn "seller send: Date, Finish"
      yield (Date 100)
    NotBuy -> I.do
      liftm $ putStrLn "seller recv: NotBuy, Finish"
      returnAt ()
```
---------------------------------------------
You can run these two examples: cabal test

result: 

```shell
----------------- run Book -----------------
buyer send: haskell book
seller recv: Title
seller send: Price
buyer recv: price
buyer can buy, send Afford
seller recv: Afford
seller send: Date, Finish
buyer recv: Date, Finish
---------------- run Book1 -----------------
buyer send: haskell book
seller recv: Title
seller send: Price
buyer recv: price
buyer send price to b2
buyer2 recv: price
buyer2 send half price to buyer, Finish
buyer recv: b2 half price
buyer can buy, send Afford
seller recv: Afford
seller send: Date, Finish
buyer recv: Date, Finish

```


