module TypeCheck(typeCheck)
where

import Data.Function
import qualified Data.Map as M
import qualified Data.Set as S
import Control.Monad
import Control.Monad.Error()

import Stau
import StauTypes

typeCheck :: ([Function], [FunSig]) -> Either String ()
typeCheck ast =
  case checkSigs ast of
    Left err        -> Left $ "Type signature error: " ++ err
    Right funsigmap -> do
      let topLevelTypes = valueMapToTypeMap preludeVariables `M.union` funsigmap
          funsigmap' = M.insert "main" (TypeFun []) funsigmap
      forM_ (fst ast) $ \f -> do
        case M.lookup (getFunName f) funsigmap' of
          Nothing  -> Left $ "Type signature for function `" ++ getFunName f ++ "' not found"
          Just sig ->
            case expType (topLevelTypes `M.union` paramTypes f sig) (getFunExp f) of
              Left err -> Left $ "Type error in function `" ++ getFunName f ++ "': " ++ err
              Right _  -> return ()

checkSigs :: ([Function], [FunSig]) -> Either String TypeMap
checkSigs (funs, sigs) =
  let funset = S.fromList (map getFunName funs)
      sigset = S.fromList (map getFunSigName sigs)
  in if sigset `S.isProperSubsetOf` funset
       then case funSigTypes sigs of
              Left err -> Left $ "Invalid type signature: " ++ err
              Right tm -> return tm
       else Left $ "Type signature without a function"

funSigTypes :: [FunSig] -> Either String TypeMap
funSigTypes fs = do
  allSigs <- mapM funSigType fs
  return $ M.fromList $ zip (map getFunSigName fs) allSigs

funSigType :: FunSig -> Either String ExpType
funSigType (FunSig _ typenames) = do
  types <- mapM getTypeByName typenames
  return $ TypeFun types

paramTypes :: Function -> ExpType -> TypeMap
paramTypes fun (TypeFun types)      = M.fromList $ zip (getFunArgs fun) types
paramTypes _   _                    = M.empty

getTypeByName :: String -> Either String ExpType
getTypeByName "Int"  = Right TypeInt
getTypeByName "Bool" = Right TypeBool
getTypeByName n      = Left $ "Invalid type name: `" ++ n ++ "'"

valueMapToTypeMap :: ValueMap -> TypeMap
valueMapToTypeMap = M.map valueToType

valueToType :: Value -> ExpType
valueToType (StackValue _ e) = e
valueToType (ExpValue   _ e) = e

isNumericType :: ExpType -> Bool
isNumericType TypeInt = True
isNumericType _       = False

type TypeMap = M.Map String ExpType

expTypeNumeric :: TypeMap -> Exp -> Exp -> Either String ExpType
expTypeNumeric em e1 e2 = do
  et1 <- expType em e1
  guardE (Right et1 == expType em e2) $ varTypeError "numeric types" em e1 e2
  guardE (isNumericType et1) $ "Not a numeric type: " ++ showExpType em e1
  return et1

compTypeError :: TypeMap -> Exp -> Exp -> String
compTypeError = varTypeError "comparison"

varTypeError :: String -> TypeMap -> Exp -> Exp -> String
varTypeError msg tm e1 e2 =
  let et1 = showExpType tm e1
      et2 = showExpType tm e2
  in typeError msg et1 et2

typeError :: String -> String -> String -> String
typeError msg e1 e2 =
  "Type error: " ++ msg ++ ": Expression has type " ++ show e1 ++ " - expected " ++ show e2

showExpType :: TypeMap -> Exp -> String
showExpType tm e1 = either (const "<unknown>") show (expType tm e1)

expType :: TypeMap -> Exp -> Either String ExpType
expType em (Plus e1 e2) = expTypeNumeric em e1 e2
expType em (Minus e1 e2) = expTypeNumeric em e1 e2
expType em (Times e1 e2) = expTypeNumeric em e1 e2
expType em (Div e1 e2) = expTypeNumeric em e1 e2
expType em (CmpEq e1 e2) = guardE (on (==) (expType em) e1 e2) (compTypeError em e1 e2) >> Right TypeBool
expType em (CmpLt e1 e2) = guardE (on (==) (expType em) e1 e2) (compTypeError em e1 e2) >> Right TypeBool
expType em (CmpLe e1 e2) = guardE (on (==) (expType em) e1 e2) (compTypeError em e1 e2) >> Right TypeBool
expType _  (Int _) = Right TypeInt
expType em (FunApp n params) = case M.lookup n em of
                                 Nothing -> Left $ "Type error on variable: `" ++ n ++ "'"
                                 Just (TypeFun expectedParams) ->
                                   let numparams = length params
                                       numExpectedParams = length expectedParams - 1
                                   in if numparams == numExpectedParams
                                        then do
                                          partypes <- mapM (expType em) params
                                          if and $ zipWith (==) partypes expectedParams
                                            then Right $ last expectedParams
                                            else Left $ "Type error on parameters to `" ++ n ++ "'"
                                        else
                                          Left $ concat ["Invalid number of parameters supplied to function `",
                                                         n, "': found ", show numparams,
                                                         " - expected ", show numExpectedParams]
                                 Just e -> if null params
                                             then Right e
                                             else Left $ "Function application on non-function `" ++ n ++ "'"
expType em (Brack e) = expType em e
expType em (Negate e) = do
  et <- expType em e
  guardE (isNumericType et) "Type error on numeric type comparison"
  return et
expType em (IfThenElse e1 e2 e3) = do
  et1 <- expType em e1
  et2 <- expType em e2
  et3 <- expType em e3
  guardE (et1 == TypeBool) $ typeError "`if' predicate" "Bool" $ showExpType em e1
  guardE (et2 == et3) $ typeError "`if' branches" (showExpType em e2) (showExpType em e3)
  return et2

guardE :: Bool -> String -> Either String ()
guardE True  _ = return ()
guardE False e = fail e


