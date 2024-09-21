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

module TypedSession.TH (protocol, protocol') where

import Control.Monad (forM, when)
import Data.Either (fromRight)
import Data.IFunctor (Sing, SingI (..))
import Data.Kind
import qualified Data.Set as Set
import Data.Traversable (for)
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

protDecsAndMsgDecs :: forall r bst. (Show r, Show bst, Enum r, Bounded r) => String -> Name -> Name -> PipeResult r bst -> Q [Dec]
protDecsAndMsgDecs protN roleName bstName PipeResult{msgT, msgT1, dnySet, stList, branchResultTypeInfo, branchFunList, allMsgBATypes} = do
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
      dataProt = [DataD [] protName [] Nothing [genConstr i | i <- stList] []]

  let tAnyToType :: Name -> T bst -> TH.Type
      tAnyToType s = \case
        TNum i -> PromotedT (mkSiName i)
        BstList i bst -> AppT (PromotedT (mkSiName i)) (PromotedT (mkName (show bst)))
        TAny i -> AppT (PromotedT (mkSiName i)) (VarT s)
        TEnd -> PromotedT $ mkName "End"
      mkArgs args =
        [ ( Bang NoSourceUnpackedness NoSourceStrictness
          , case ag of
              [] -> error "np"
              (x : xs) -> foldl' AppT (ConT (mkName x)) (map (ConT . mkName) xs)
          )
        | ag <- args
        ]
      typeListT :: [TH.Type] -> TH.Type
      typeListT = foldl1 AppT

      isTAny :: T bst -> Bool
      isTAny = \case
        TAny _ -> True
        _ -> False

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
      dataSingletonProt = [DataD [] protSName [KindedTV aVar BndrReq (ConT protName)] Nothing [genSConstr i | i <- stList] []]

  -- generate type family Sing to Singleton protocol
  let singSingletonProt = [TySynInstD (TySynEqn Nothing (ConT ''Sing) (ConT protSName))]

  aVar1 <- newName "a"
  let branchResultDatas =
        [ DataD
            []
            dataName
            [KindedTV aVar1 BndrReq (ConT protName)]
            Nothing
            [ GadtC [constrName] (mkArgs args) (AppT (ConT dataName) (tAnyToType (mkName "s") t))
            | (bst, args, t) <- constrs
            , let constrName = mkName ("BranchSt_" <> show bst)
            ]
            []
        | (name, constrs) <- branchResultTypeInfo
        , let dataName = mkName name
        ]
  branchFunTypes <- for branchFunList $ \(r, st, t) -> do
    mVar <- newName "m"
    pure
      ( TySynD
          (mkName (st <> "Fun"))
          [KindedTV mVar BndrReq (AppT (AppT ArrowT StarT) StarT)]
          ( typeListT
              [ ConT ''TSC.Peer
              , ConT roleName
              , ConT protName
              , ConT (mkName (show r))
              , VarT mVar
              , ConT (mkName st)
              , (tAnyToType (mkName "s") t)
              ]
          )
      )
  let firstTList = case msgT of
        Msg (ts, _, _) _ _ _ _ :> _ -> ts
        Label (ts, _) _ :> _ -> ts
        Branch ts _ _ _ -> ts
        Goto (ts, _) _ -> ts
        Terminal ts -> ts

      mkTySynDFun name t =
        if isTAny t
          then do
            sVar1 <- newName @Q "s"
            pure (TySynD name [KindedTV sVar1 BndrReq (ConT bstName)] (tAnyToType sVar1 t))
          else pure (TySynD name [] (tAnyToType (mkName "s") t))

      mkAllRoleTySynDFun nameFun ts =
        for (zip [minBound @r .. maxBound] ts) $
          \(r, t) -> mkTySynDFun (nameFun r) t

  roleStartSts <- mkAllRoleTySynDFun (\r -> (mkName (show r <> "StartSt"))) firstTList

  allMsgBADecs <-
    concat <$> do
      for allMsgBATypes $ \(cname, beforeSt, afterSt) -> do
        bfs <- mkAllRoleTySynDFun (\r -> mkName (show r <> "Before" <> cname <> "St")) beforeSt
        afs <- mkAllRoleTySynDFun (\r -> mkName (show r <> "After" <> cname <> "St")) afterSt
        pure (bfs <> afs)

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
        | i <- stList
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

  let mkDataInstanceMsg :: Name -> Protocol (MsgT1 r bst) r bst -> Q [Con]
      mkDataInstanceMsg s = \case
        Msg ((a, b, c), (from, to), _) constr args _ _ :> prots -> do
          let mkTName =
                typeListT
                  [ ConT ''TSC.Msg
                  , ConT roleName
                  , ConT protName
                  , tAnyToType s a
                  , PromotedT (mkName (show from))
                  , tAnyToType s b
                  , PromotedT (mkName (show to))
                  , tAnyToType s c
                  ]
          let val =
                let gadtc =
                      GadtC
                        [mkName constr]
                        (mkArgs args)
                        mkTName
                 in if any isTAny [a, b, c]
                      then ForallC [KindedTV s SpecifiedSpec (ConT bstName)] [] gadtc
                      else gadtc
          vals <- mkDataInstanceMsg s prots
          pure (val : vals)
        Label _ _ :> prots -> mkDataInstanceMsg s prots
        Branch _ _ _ ls -> do
          ls' <- forM ls $ \(BranchSt _ _ _ prot) -> mkDataInstanceMsg s prot
          pure $ concat ls'
        _ -> pure []

  -- make data instance Msg
  ssVar <- newName "s"
  instMsgDesc <- mkDataInstanceMsg ssVar (msgT1)
  fromVar <- newName "from"
  sendVar <- newName "send"
  sendNewStVar <- newName "sendNewSt"
  recvVar <- newName "recv"
  receiverNewStVar <- newName "receiverNewSt"
  let instanceMsg =
        [ InstanceD
            Nothing
            []
            (AppT (AppT (ConT ''TSC.Protocol) (ConT roleName)) (ConT protName))
            ( instDoneDesc
                ++ [ DataInstD
                      []
                      ( Just
                          [ KindedTV fromVar () (ConT protName)
                          , KindedTV sendVar () (ConT roleName)
                          , KindedTV sendNewStVar () (ConT protName)
                          , KindedTV recvVar () (ConT roleName)
                          , KindedTV receiverNewStVar () (ConT protName)
                          ]
                      )
                      ( typeListT
                          [ ConT ''TSC.Msg
                          , ConT roleName
                          , ConT protName
                          , (SigT (VarT fromVar) (ConT protName))
                          , (SigT (VarT sendVar) (ConT roleName))
                          , (SigT (VarT sendNewStVar) (ConT protName))
                          , (SigT (VarT recvVar) (ConT roleName))
                          , (SigT (VarT receiverNewStVar) (ConT protName))
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
      ++ branchResultDatas
      ++ branchFunTypes
      ++ roleStartSts
      ++ allMsgBADecs
      ++ instanceSingI
      ++ instanceSingToInt
      ++ instanceMsg

protocol'
  :: forall r bst
   . ( Enum r
     , Bounded r
     , Show r
     , Enum bst
     , Bounded bst
     , Show bst
     , Ord r
     )
  => String -> Name -> Name -> Bool -> QuasiQuoter
protocol' protN roleName bstName b =
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
        when b $ runIO $ do
          writeFile (protN <> ".prot") graphStr
          putStrLn graphStr
        d1 <- roleDecs roleName
        d2 <- protDecsAndMsgDecs protN roleName bstName pipResult
        pure (d1 ++ d2)

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
  protocol' @r @bst protN roleName bstName False
