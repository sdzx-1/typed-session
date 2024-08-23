{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -split-sections #-}

module TypedSession.TH where

import Control.Monad (forM)
import Data.Either (fromRight)
import Data.Kind
import qualified Data.Set as Set
import Language.Haskell.TH hiding (Type)
import qualified Language.Haskell.TH as TH
import Language.Haskell.TH.Quote
import TypedSession.State.GenDoc
import TypedSession.State.Parser (runProtocolParser)
import TypedSession.State.Piple (PipleResult (..), piple)
import TypedSession.State.Render
import TypedSession.State.Type (BranchSt (..), Creat, MsgOrLabel (..), MsgT1, Protocol (..), ProtocolError, T (..))

roleDecs :: Name -> Q [Dec]
roleDecs name = do
  res <- reify name
  case res of
    TyConI (DataD [] dName [] Nothing cons _) -> do
      a <- newName "a"
      x <- newName "x"
      pure $
        [ DataD
            []
            (addS dName)
            [KindedTV a BndrReq (ConT dName)]
            Nothing
            [GadtC [addS n] [] (AppT (ConT (addS dName)) (PromotedT n)) | NormalC n [] <- cons]
            []
        , TySynInstD (TySynEqn Nothing (ConT (mkName "Data.IFunctor.Sing")) (ConT (addS dName)))
        ]
          ++ [ InstanceD
                Nothing
                []
                (AppT (ConT $ mkName "Data.IFunctor.SingI") (PromotedT n))
                [FunD (mkName "sing") [Clause [] (NormalB (ConE (addS n))) []]]
             | NormalC n [] <- cons
             ]
          ++ [ InstanceD
                Nothing
                []
                (AppT (ConT (mkName "TypedSession.Core.SingToInt")) (ConT name))
                [ FunD
                    (mkName "singToInt")
                    [Clause [VarP x] (NormalB (AppE (ConE (mkName "I#")) (AppE (VarE $ mkName "dataToTag#") (VarE x)))) []]
                ]
             ]
    _ -> error "np"

addS :: Name -> Name
addS name =
  let n = (nameBase name)
   in mkName ("S" <> n)

protDecsAndMsgDecs :: forall r bst. (Show r, Show bst) => String -> Name -> Name -> PipleResult r bst -> Q [Dec]
protDecsAndMsgDecs protN roleName bstName PipleResult{msgT1, dnySet, stBound = (fromVal, toVal)} = do
  sVar <- newName "s"
  let protName = mkName protN
      protSName = mkName ("S" <> protN)
      genConstr i =
        if i == -1
          then NormalC (mkName "End") []
          else
            if i `Set.member` dnySet
              then
                NormalC
                  (mkName $ "S" <> show i)
                  [(Bang NoSourceUnpackedness NoSourceStrictness, ConT bstName)]
              else NormalC (mkName $ "S" <> show i) []
      genSConstr i =
        if i == -1
          then
            GadtC [mkName "SEnd"] [] (AppT (ConT protSName) (PromotedT (mkName "End")))
          else
            if i `Set.member` dnySet
              then
                ForallC
                  [KindedTV sVar SpecifiedSpec (ConT bstName)]
                  []
                  ( GadtC
                      [mkName $ "SS" <> show i]
                      []
                      (AppT (ConT protSName) (AppT (PromotedT $ mkName $ "S" <> show i) (VarT sVar)))
                  )
              else
                GadtC [mkName $ "SS" <> show i] [] (AppT (ConT protSName) (PromotedT $ mkName $ "S" <> show i))

      isTAny :: T bst -> Bool
      isTAny = \case
        TAny _ -> True
        _ -> False

      mkInstanceMsg :: Name -> Protocol (MsgT1 r bst) r bst -> Q [Con]
      mkInstanceMsg s = \case
        Msg ((a, b, c), (from, to), _) constr args _ _ :> prots -> do
          let tAnyToType :: T bst -> TH.Type
              tAnyToType = \case
                TNum i -> PromotedT (mkName $ "S" <> show i)
                BstList i bst -> AppT (PromotedT (mkName $ "S" <> show i)) (PromotedT (mkName (show bst)))
                TAny i -> AppT (PromotedT (mkName $ "S" <> show i)) (VarT s)
                TEnd -> PromotedT $ mkName "End"

          let mkTName =
                ( AppT
                    ( AppT
                        ( AppT
                            ( AppT
                                ( AppT
                                    (ConT $ mkName "Msg")
                                    (ConT roleName)
                                )
                                (ConT protName)
                            )
                            (tAnyToType a)
                        )
                        (AppT (AppT (PromotedTupleT 2) (PromotedT (mkName (show from)))) (tAnyToType b))
                    )
                    (AppT (AppT (PromotedTupleT 2) (PromotedT (mkName (show to)))) (tAnyToType c))
                )

          let val =
                let gadtc =
                      GadtC
                        [mkName constr]
                        [ (Bang NoSourceUnpackedness NoSourceStrictness, ConT (mkName ag))
                        | ag <- args
                        ]
                        mkTName
                 in if any isTAny [a, b, c]
                      then
                        ForallC
                          [KindedTV s SpecifiedSpec (ConT bstName)]
                          []
                          gadtc
                      else gadtc
          res <- mkInstanceMsg s prots
          pure (val : res)
        Label _ _ :> prots -> mkInstanceMsg s prots
        Branch _ _ ls -> do
          ls' <- forM ls $ \(BranchSt _ _ prot) -> mkInstanceMsg s prot
          pure $ concat ls'
        _ -> pure []

  a <- newName "a"
  s1 <- newName "s1"
  x <- newName "x"

  -- make instance Done
  res <- reify roleName
  instDoneDesc <- case res of
    TyConI (DataD [] _ [] Nothing cons _) -> do
      pure
        [ TySynInstD
            ( TySynEqn
                Nothing
                (AppT (ConT (mkName "Done")) (PromotedT n))
                (PromotedT (mkName "End"))
            )
        | NormalC n [] <- cons
        ]
    _ -> error "np"

  -- make instance msg
  ss <- newName "s"
  instMsgDesc <- mkInstanceMsg ss (msgT1)

  fromVar <- newName "from"
  sendVar <- newName "send"
  recvVar <- newName "recv"
  pure $
    [ DataD [] protName [] Nothing [genConstr i | i <- [fromVal .. toVal]] []
    ]
      ++ [DataD [] protSName [KindedTV a BndrReq (ConT protName)] Nothing [genSConstr i | i <- [fromVal .. toVal]] []]
      ++ [TySynInstD (TySynEqn Nothing (ConT (mkName "Data.IFunctor.Sing")) (ConT protSName))]
      ++ [ InstanceD
            Nothing
            []
            ( AppT
                (ConT $ mkName "Data.IFunctor.SingI")
                ( if i == -1
                    then PromotedT (mkName "End")
                    else
                      if i `Set.member` dnySet
                        then SigT (AppT (PromotedT (mkName ("S" <> show i))) (VarT s1)) (ConT protName)
                        else PromotedT (mkName ("S" <> show i))
                )
            )
            [ FunD
                (mkName "sing")
                [Clause [] (NormalB (ConE (mkName $ "S" <> (if i == -1 then "End" else ("S" <> show i))))) []]
            ]
         | i <- [fromVal .. toVal]
         ]
      ++ [ InstanceD
            Nothing
            []
            (AppT (ConT (mkName "TypedSession.Core.SingToInt")) (ConT protName))
            [ FunD
                (mkName "singToInt")
                [Clause [VarP x] (NormalB (AppE (ConE (mkName "I#")) (AppE (VarE $ mkName "dataToTag#") (VarE x)))) []]
            ]
         ]
      ++ [ InstanceD
            Nothing
            []
            ( AppT
                (AppT (ConT (mkName "TypedSession.Core.Protocol")) (ConT roleName))
                (ConT protName)
            )
            ( instDoneDesc
                ++ let ct1 = (AppT (AppT (TupleT 2) (ConT roleName)) (ConT protName))
                    in [ DataInstD
                          []
                          ( Just
                              [ KindedTV fromVar () (ConT protName)
                              , KindedTV sendVar () ct1
                              , KindedTV recvVar () ct1
                              ]
                          )
                          ( AppT
                              ( AppT
                                  ( AppT
                                      ( AppT
                                          ( AppT
                                              (ConT $ mkName "Msg")
                                              (ConT roleName)
                                          )
                                          (ConT protName)
                                      )
                                      (SigT (VarT fromVar) (ConT protName))
                                  )
                                  (SigT (VarT sendVar) ct1)
                              )
                              (SigT (VarT recvVar) ct1)
                          )
                          Nothing
                          instMsgDesc
                          []
                       ]
            )
         ]

protocol
  :: forall r bst
   . ( Enum r
     , Bounded r
     , Show r
     , Enum bst
     , Bounded bst
     , Show bst
     , Ord r
     )
  => String -> Name -> Name -> QuasiQuoter
protocol protN roleName bstName =
  QuasiQuoter
    { quoteExp = const $ fail "No protocol parse for exp"
    , quotePat = const $ fail "No protocol parse for pat"
    , quoteType = const $ fail "No protocol parser for type"
    , quoteDec = parseOrThrow
    }
 where
  parseOrThrow :: String -> Q [Dec]
  parseOrThrow st = case runProtocolParser @r @bst st of
    Left e -> fail (show e)
    Right protCreat -> case piple protCreat of
      Left e -> fail (show e)
      Right pipResult -> do
        let graphStr = genGraph @r @bst defaultStrFilEnv pipResult
        runIO $ do
          writeFile (protN <> ".prot") graphStr
          putStrLn graphStr
        d1 <- roleDecs roleName
        d2 <- protDecsAndMsgDecs protN roleName bstName pipResult
        pure (d1 ++ d2)
