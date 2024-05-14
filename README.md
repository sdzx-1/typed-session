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
    :S12'                                                       :S12 s

   ---------------------------------------------------------------------
   |:S12 EnoughBudget                                          :S12 s
   | <                  Afford ->                               >
   |:S3                                                        :S3
   | <                  <- Data Int                             >
   |:End                                                       :End
   ---------------------------------------------------------------------

   ---------------------------------------------------------------------
   |:S12 NotEnoughBuget                                        :S12 s
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
    :S11                                                       :S12 s                  :S11
     <                                  PriceToBuyer2 Int ->                            >
    :S110                                                                              :S110
     <                                  <- HalfPrice  Int                               >
    :S12'                                                                               :End

   ---------------------------------------------------------------------
   |:S12 EnoughBudget                                          :S12 s
   | <                  Afford ->                               >
   |:S3                                                        :S3
   | <                  <- Data Int                             >
   |:End                                                       :End
   ---------------------------------------------------------------------

   ---------------------------------------------------------------------
   |:S12  NotEnoughBuget                                       :S12
   | <                  NotBuy ->                               >
   |:End                                                       :End
   ---------------------------------------------------------------------

-}
```
[Code:](https://github.com/sdzx-1/typed-communication-protocol/blob/main/test/Book1.hs)
```haskell

data Role = Buyer | Seller | Buyer2
  deriving (Show, Eq, Ord)

data BudgetSt
  = EnoughBudget
  | NotEnoughBuget

data BookSt
  = S0
  | S1
  | S11
  | S110
  | S12'
  | S12 BudgetSt
  | S3
  | End

type Date = Int

instance Protocol Role BookSt where
  type Done Buyer = End
  type Done Seller = End
  type Done Buyer2 = End
  data Msg Role BookSt from send recv  where
    Title     :: String -> Msg Role BookSt S0                   '(Buyer, S1)       '(Seller, S1) 
    Price     :: Int ->    Msg Role BookSt S1                   '(Seller, S12 s)   '(Buyer, S11) 
    PriceToB2 :: Int ->    Msg Role BookSt S11                  '(Buyer  ,S110)    '(Buyer2 ,S110) 
    HalfPrice :: Int ->    Msg Role BookSt S110                 '(Buyer2 ,End)     '(Buyer  ,S12') 
    Afford    ::           Msg Role BookSt (S12 EnoughBudget)   '(Buyer  ,S3)      '(Seller ,S3) 
    Date      :: Date ->   Msg Role BookSt S3                   '(Seller ,End)     '(Buyer  ,End)
    NotBuy    ::           Msg Role BookSt (S12 NotEnoughBuget) '(Buyer  ,End)     '(Seller ,End)

budget :: Int
budget = 16

data CheckPriceResult :: BookSt -> Type where
  Yes :: CheckPriceResult (S12 EnoughBudget)
  No :: CheckPriceResult (S12 NotEnoughBuget)

checkPrice :: Int -> Int -> Peer Role BookSt Buyer IO CheckPriceResult S12'
checkPrice i h = 
  if i <= budget + h
  then LiftM $ pure (ireturn Yes)
  else LiftM $ pure (ireturn No)

buyerPeer
  :: Peer Role BookSt Buyer IO (At (Maybe Date) (Done Buyer)) S0
buyerPeer = I.do
  yield (Title "haskell book")
  Recv (Price i) <- await
  yield (PriceToB2 i)
  Recv (HalfPrice hv) <- await
  res <- checkPrice i hv
  case res of
    Yes -> I.do
      yield Afford
      Recv (Date d) <- await
      returnAt (Just d)
    No -> I.do
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
    Afford -> I.do
      yield (Date 100)
    NotBuy -> I.do
      returnAt ()

```
---------------------------------------------
You can run these examples: cabal test

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

---------------------------------------------
Here is the more complex book2. 

In book2, the seller checks whether the Title exists, 
and the buyer will notify buyer2 of various results.

```haskell
{-

-----------------------------------------------------------------------------------------------
    Buyer                                                      Seller                  Buyer2
    :S0                                                        :S0                      :S11 s
     <                     Title String  ->                     >
    :S1 s                                                      :S1'

 ------------------------------------------------------------------------------------------
 |  :S1 s                                                      :S1 BookNotFound
 |   <                     <-  BookNotFound                     >
 |  :S11 BookNotFound                                          :End                    :S11 s
 |   <                                  SellerNotFoundBook ->                            >
 |  :End                                                                               :End
 ------------------------------------------------------------------------------------------

 ------------------------------------------------------------------------------------------
 |  :S1 s                                                      :S1 BookFound
 |   <                     <-  Price Int                         >
 |  :S11 BookFound                                             :S12 s                  :S11 s
 |   <                                  PriceToBuyer2 Int ->                            >
 |  :S110                                                                              :S110
 |   <                                  <- HalfPrice  Int                               >
 |  :S12'                                                                              :S113 s
 |
 | ----------------------------------------------------------------------------------------
 | |:S12  EnoughBudget                                         :S12 s
 | | <                  Afford ->                               >
 | |:S3                                                        :S3
 | | <                  <- Date Int                             >
 | | :S113 EnoughtBudget                                      :End                   :S113 s
 | | <                                 Success Int  ->                                  >
 | |:End                                                                              :End
 | ----------------------------------------------------------------------------------------
 |
 | ----------------------------------------------------------------------------------------
 | |:S12  NotEnoughBuget                                       :S12 s
 | | <                  NotBuy ->                               >
 | | S113 NotEnoughBuget                                      :End                  :S113 s
 | | <                                 Failed  ->                                       >
 | |:End                                                                             :End
 | ----------------------------------------------------------------------------------------
 ------------------------------------------------------------------------------------------

-}

```
---------------------------------------------
Here is the more and more complex book3. 

In book3:
- The seller checks whether the Title exists. 
- The Buyer chooses whether he needs help from buyer2.
- The Buyer2 chooses whether to support byer.
- The Buyer will notify buyer2 of various results.

```haskell
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

```