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
-----------------------------------------------
 role: Buyer Seller

 Buyer                            Seller
       title  ->
       <-  price
       checkPrice
            CheckTrue    Afford ->
                         data <-

            CheckFalse   NotBuy ->

------------------- Add State ---------------------------
 Buyer                                                      Seller
  :S0                                                        :S0
                     Title  ->
  :S1                                                        :S1
                     <-  Price
  :S12
                                                             :S2 s

     :[S2 True]       Afford ->
         :S3                                             :S3
                      Data <-
         :End                                            :End

     :[S2 False]      NotBuy ->
         :End                                            :End
 - -}

```
[Code:](https://github.com/sdzx-1/typed-communication-protocol/blob/main/test/Book.hs) 
```haskell
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
```
-----------------------------------------


 two-buyer bookseller protocol



<img width="597" alt="屏幕截图 2024-04-22 231527" src="https://github.com/sdzx-1/typed-communication-protocol/assets/63328943/c7f11579-74fd-481e-bf70-140caddfa25e">

```haskell
{-

-----------------------------------------------
 Buyer                                                   Seller                               Buyer2
  :S0                                                        :S0
                    Title  ->
  :S1                                                        :S1
                    <-  Price
  :S11                                                                                           :S11
                                                                         -> PriceToB2
  :S110                                                                                          :S110
                                                                         <- HalePrice
  :S12                                                                                           :End

                                                            :S2 s

     :[S2 True]       Afford ->
         :S3                                         :S3
                      Data <-
         :End                                        :End

     :[S2 False]      NotBuy ->
         :End                                        :End

-}
```
[Code:](https://github.com/sdzx-1/typed-communication-protocol/blob/main/test/Book1.hs)
```haskell
data Role = Buyer | Seller | Buyer2

data BookSt
  = S0
  | S1
  | S11
  | S110
  | S12
  | S2 Bool
  | S3
  | End

type Date = Int

instance Protocol Role BookSt where
  type Done Buyer = End
  type Done Seller = End
  type Done Buyer2 = End
  data Msg Role BookSt send recv from to where
    Title :: String -> Msg Role BookSt Buyer Seller S0 '(S1, S1)
    Price :: Int -> Msg Role BookSt Seller Buyer S1 '(S2 s, S11)
    PriceToB2 :: Int -> Msg Role BookSt Buyer Buyer2 S11 '(S110, S110)
    HalfPrice :: Int -> Msg Role BookSt Buyer2 Buyer S110 '(End, S12)
    Afford :: Msg Role BookSt Buyer Seller (S2 True) '(S3, S3)
    Date :: Date -> Msg Role BookSt Seller Buyer S3 '(End, End)
    NotBuy :: Msg Role BookSt Buyer Seller (S2 False) '(End, End)

data CheckPriceResult :: BookSt -> Type where
  CheckTrue :: CheckPriceResult (S2 True)
  CheckFalse :: CheckPriceResult (S2 False)

budget :: Int
budget = 100

checkPrice :: Int -> Int -> Peer Role BookSt Buyer IO CheckPriceResult S12
checkPrice i hv =
  if i < budget + hv
    then LiftM $ pure (ireturn CheckTrue)
    else LiftM $ pure (ireturn CheckFalse)

buyerPeer
  :: Peer Role BookSt Buyer IO (At (Maybe Date) (Done Buyer)) S0
buyerPeer = I.do
  yield (Title "haskell book")
  Recv (Price i) <- await
  yield (PriceToB2 i)
  Recv (HalfPrice hv) <- await
  res <- checkPrice i hv
  case res of
    CheckTrue -> I.do
      yield Afford
      Recv (Date d) <- await
      returnAt (Just d)
    CheckFalse -> I.do
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


