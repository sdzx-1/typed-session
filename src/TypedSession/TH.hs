{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MagicHash #-}
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
import Data.IFunctor (Sing, SingI (..))
import Data.Kind
import qualified Data.Set as Set
import GHC.Exts (DataToTag (..), Int (..))
import Language.Haskell.TH hiding (Type)
import qualified Language.Haskell.TH as TH
import Language.Haskell.TH.Quote
import TypedSession.Core (SingToInt (..))
import qualified TypedSession.Core as TSC
import TypedSession.State.Parser (runProtocolParser)
import TypedSession.State.Pipeline (PipeResult (..), genGraph, pipe)
import TypedSession.State.Render
import TypedSession.State.Type (BranchSt (..), Creat, MsgOrLabel (..), MsgT1, Protocol (..), ProtocolError, T (..))

roleDecs :: Name -> Q [Dec]
roleDecs name = do
  res <- reify name
  case res of
    TyConI (DataD [] dName [] Nothing cons _) -> do
      aVar <- newName "a"
      xVar <- newName "x"
      let
        addPerfixS :: Name -> Name
        addPerfixS vname =
          let n = (nameBase vname)
           in mkName ("S" <> n)

        roleSingleton =
          [ DataD
              []
              (addPerfixS dName)
              [KindedTV aVar BndrReq (ConT dName)]
              Nothing
              [GadtC [addPerfixS n] [] (AppT (ConT (addPerfixS dName)) (PromotedT n)) | NormalC n [] <- cons]
              []
          ]
        singToSRole = [TySynInstD (TySynEqn Nothing (ConT ''Sing) (ConT (addPerfixS dName)))]
        instanceSingI =
          [ InstanceD
              Nothing
              []
              (AppT (ConT ''SingI) (PromotedT n))
              [FunD 'sing [Clause [] (NormalB (ConE (addPerfixS n))) []]]
          | NormalC n [] <- cons
          ]
        instanceSingToInt =
          [ InstanceD
              Nothing
              []
              (AppT (ConT ''SingToInt) (ConT name))
              [ FunD
                  'singToInt
                  [Clause [VarP xVar] (NormalB (AppE (ConE 'I#) (AppE (VarE 'dataToTag#) (VarE xVar)))) []]
              ]
          ]
      pure $ roleSingleton ++ singToSRole ++ instanceSingI ++ instanceSingToInt
    _ -> error $ "Name: " ++ show name ++ " is not a data constructor"

protDecsAndMsgDecs :: forall r bst. (Show r, Show bst) => String -> Name -> Name -> PipeResult r bst -> Q [Dec]
protDecsAndMsgDecs protN roleName bstName PipeResult{msgT1, dnySet, stBound = (fromVal, toVal)} = do
  let protName = mkName protN
      protSName = mkName ("S" <> protN)
      mkSiName i = mkName $ "S" <> show i
      mkSSiName i = mkName $ "SS" <> show i
      genConstr i =
        if i == -1
          then NormalC (mkName "End") []
          else
            if i `Set.member` dnySet
              then
                NormalC
                  (mkSiName i)
                  [(Bang NoSourceUnpackedness NoSourceStrictness, ConT bstName)]
              else NormalC (mkSiName i) []
      -- generate protocol data type
      dataProt = [DataD [] protName [] Nothing [genConstr i | i <- [fromVal .. toVal]] []]

  sVar <- newName "s"
  aVar <- newName "a"
  let genSConstr i =
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
                      [mkSSiName i]
                      []
                      (AppT (ConT protSName) (AppT (PromotedT $ mkSiName i) (VarT sVar)))
                  )
              else
                GadtC [mkSSiName i] [] (AppT (ConT protSName) (PromotedT $ mkSiName i))
      -- generate protocol singleton data type
      dataSingletonProt = [DataD [] protSName [KindedTV aVar BndrReq (ConT protName)] Nothing [genSConstr i | i <- [fromVal .. toVal]] []]
  -- generate type family Sing to Singleton protocol
  let singSingletonProt = [TySynInstD (TySynEqn Nothing (ConT ''Sing) (ConT protSName))]

  s1 <- newName "s1"
  -- generate instance SingI
  let instanceSingI =
        [ InstanceD
            Nothing
            []
            ( AppT
                (ConT ''SingI)
                ( if i == -1
                    then PromotedT (mkName "End")
                    else
                      if i `Set.member` dnySet
                        then SigT (AppT (PromotedT (mkSiName i)) (VarT s1)) (ConT protName)
                        else PromotedT (mkSiName i)
                )
            )
            [FunD 'sing [Clause [] (NormalB (ConE (mkName $ "S" <> (if i == -1 then "End" else ("S" <> show i))))) []]]
        | i <- [fromVal .. toVal]
        ]

  xVar <- newName "x"
  -- generate instance SingToInt
  let instanceSingToInt =
        [ InstanceD
            Nothing
            []
            (AppT (ConT ''SingToInt) (ConT protName))
            [ FunD 'singToInt [Clause [VarP xVar] (NormalB (AppE (ConE 'I#) (AppE (VarE 'dataToTag#) (VarE xVar)))) []]
            ]
        ]

  -- make instance type family Done
  roleRes <- reify roleName
  instDoneDesc <- case roleRes of
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
    _ -> error $ "Name: " ++ show roleName ++ " is not a data constructor"

  let typeListT :: [TH.Type] -> TH.Type
      typeListT = foldl1 AppT

      isTAny :: T bst -> Bool
      isTAny = \case
        TAny _ -> True
        _ -> False

      mkDataInstanceMsg :: Name -> Protocol (MsgT1 r bst) r bst -> Q [Con]
      mkDataInstanceMsg s = \case
        Msg ((a, b, c), (from, to), _) constr args _ _ :> prots -> do
          let tAnyToType :: T bst -> TH.Type
              tAnyToType = \case
                TNum i -> PromotedT (mkSiName i)
                BstList i bst -> AppT (PromotedT (mkSiName i)) (PromotedT (mkName (show bst)))
                TAny i -> AppT (PromotedT (mkSiName i)) (VarT s)
                TEnd -> PromotedT $ mkName "End"
          let mkTName =
                typeListT
                  [ ConT ''TSC.Msg
                  , ConT roleName
                  , ConT protName
                  , tAnyToType a
                  , typeListT [PromotedTupleT 2, PromotedT (mkName (show from)), tAnyToType b]
                  , typeListT [PromotedTupleT 2, PromotedT (mkName (show to)), tAnyToType c]
                  ]
          let val =
                let gadtc =
                      GadtC
                        [mkName constr]
                        [ ( Bang NoSourceUnpackedness NoSourceStrictness
                          , case words ag of
                              [] -> error "np"
                              (x : xs) -> foldl' AppT (ConT (mkName x)) (map (ConT . mkName) xs)
                          )
                        | ag <- args
                        ]
                        mkTName
                 in if any isTAny [a, b, c]
                      then ForallC [KindedTV s SpecifiedSpec (ConT bstName)] [] gadtc
                      else gadtc
          vals <- mkDataInstanceMsg s prots
          pure (val : vals)
        Label _ _ :> prots -> mkDataInstanceMsg s prots
        Branch _ _ ls -> do
          ls' <- forM ls $ \(BranchSt _ _ prot) -> mkDataInstanceMsg s prot
          pure $ concat ls'
        _ -> pure []

  -- make data instance Msg
  ssVar <- newName "s"
  instMsgDesc <- mkDataInstanceMsg ssVar (msgT1)
  fromVar <- newName "from"
  sendVar <- newName "send"
  recvVar <- newName "recv"
  let instanceMsg =
        [ InstanceD
            Nothing
            []
            (AppT (AppT (ConT ''TSC.Protocol) (ConT roleName)) (ConT protName))
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
                          ( typeListT
                              [ ConT ''TSC.Msg
                              , ConT roleName
                              , ConT protName
                              , (SigT (VarT fromVar) (ConT protName))
                              , (SigT (VarT sendVar) ct1)
                              , (SigT (VarT recvVar) ct1)
                              ]
                          )
                          Nothing
                          instMsgDesc
                          []
                       ]
            )
        ]

  pure $
    dataProt
      ++ dataSingletonProt
      ++ singSingletonProt
      ++ instanceSingI
      ++ instanceSingToInt
      ++ instanceMsg

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
    Right protCreat -> case pipe protCreat of
      Left e -> fail (show e)
      Right pipResult -> do
        let graphStr = genGraph @r @bst pipResult
        runIO $ do
          writeFile (protN <> ".prot") graphStr
          putStrLn graphStr
        d1 <- roleDecs roleName
        d2 <- protDecsAndMsgDecs protN roleName bstName pipResult
        pure (d1 ++ d2)
