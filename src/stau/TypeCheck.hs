module TypeCheck(typeCheck)
where

import Data.List
import Data.Function
import qualified Data.Map as M
import qualified Data.Set as S
import Control.Monad
import Control.Monad.Error()

import Stau
import StauTypes

typeCheck :: Module -> Either String ()
typeCheck ast = do
  let datadeclset   = S.fromList (map dataDeclName (moduleDataDecls ast))
  case checkSigs datadeclset ast of
    Left err        -> Left $ "Type signature error: " ++ err
    Right funsigmap -> do
      let topLevelTypes = valueMapToTypeMap preludeVariables `M.union` funsigmap
          funsigmap'    = M.insert "main" (TypeFun []) funsigmap
          datamap       = buildDataTypeMap ast
      case buildConsMap ast of
        Left err      -> Left $ "Data type error: " ++ err
        Right consmap -> do
        forM_ (moduleFunctions ast) $ \f -> do
          case M.lookup (getFunName f) funsigmap' of
            Nothing  -> Left $ "Type signature for function `" ++ getFunName f ++ "' not found"
            Just sig ->
              case paramTypes (M.map dataDeclName datamap) datadeclset consmap f sig of
                Left err -> Left $ "Parameter declaration error for function `" ++ getFunName f ++ "': " ++ err
                Right pr ->
                  case expType datamap (topLevelTypes `M.union` pr) (getFunExp f) of
                    Left err -> Left $ "Type error in function `" ++ getFunName f ++ "': " ++ err
                    Right _  -> return ()

type ConstructorMap = M.Map String Constructor

buildConsMap :: Module -> Either String ConstructorMap
buildConsMap ast =
  let conss = concatMap dataConstructors $ moduleDataDecls ast
      consids = map constructorName conss
      consmap = M.fromList $ zip consids conss
  in case findDups consids of
       [] -> Right consmap
       ns -> Left $ "Duplicate data constructor definitions: " ++ (intercalate "\n" $ map show ns)

findDups :: (Ord a) => [a] -> [a]
findDups = map head . filter (\l -> length l > 1) . group . sort

checkSigs :: S.Set String -> Module -> Either String TypeMap
checkSigs datadeclset (Module funs sigs _) =
  let funset = S.fromList (map getFunName funs)
      sigset = S.fromList (map getFunSigName sigs)
  in if sigset `S.isProperSubsetOf` funset
       then case funSigTypes datadeclset sigs of
              Left err -> Left $ "Invalid type signature: " ++ err
              Right tm -> return tm
       else Left $ "Type signature without a function"

funSigTypes :: S.Set String -> [FunSig] -> Either String TypeMap
funSigTypes datadeclset fs = do
  allSigs <- mapM (funSigType datadeclset) fs
  return $ M.fromList $ zip (map getFunSigName fs) allSigs

funSigType :: S.Set String -> FunSig -> Either String ExpType
funSigType datadeclset (FunSig _ typenames) = do
  types <- mapM (getTypeByName datadeclset) typenames
  return $ TypeFun types

paramTypes :: M.Map String String -> S.Set String -> ConstructorMap -> Function -> ExpType -> Either String TypeMap
paramTypes _ _ _ _ (TypeFun []) = Right M.empty -- main
paramTypes datamap datadeclset consmap fun (TypeFun types) = liftM M.fromList (paramTypes' (getFunArgs fun) (init types))
  where paramTypes' :: [ParamDecl] -> [ExpType] -> Either String [(String, ExpType)]
        paramTypes' [] [] = Right []
        paramTypes' xs [] = Left $ "Not enough parameter declarations: " ++ show xs
        paramTypes' [] _  = Left $ "Too many parameter declarations"
        paramTypes' (VariableParam n : params)         (et:ets) = do
          let this = (n, et)
          rest <- paramTypes' params ets
          return (this : rest)
        paramTypes' (ConstructorParam dc cps : params) (et:ets)  =
          case M.lookup dc consmap of
            Nothing                  -> Left $ "Unknown data constructor: `" ++ dc ++ "'"
            Just (Constructor _ dft) -> do
              constypes <- mapM (getTypeByName datadeclset) dft
              guardE (Just (show et) == M.lookup (show et) datamap) $ "Data type `" ++ show et ++ "' does not match constructor `" ++ dc ++ "'"
              this <- paramTypes' cps $ constypes
              rest <- paramTypes' params ets
              return (this ++ rest)
paramTypes _ _ _ _ _ = Left $ "Internal compiler error in paramTypes"

getTypeByName :: S.Set String -> String -> Either String ExpType
getTypeByName _           "Int"  = Right TypeInt
getTypeByName _           "Bool" = Right TypeBool
getTypeByName datadeclset n      = 
  if S.member n datadeclset
    then Right (CustomType n)
    else Left $ "Invalid type name: `" ++ n ++ "'"

valueMapToTypeMap :: ValueMap -> TypeMap
valueMapToTypeMap = M.map valueToType

valueToType :: Value -> ExpType
valueToType (StackValue _ e) = e
valueToType (ExpValue   _ e) = e

isNumericType :: ExpType -> Bool
isNumericType TypeInt = True
isNumericType _       = False

type TypeMap = M.Map String ExpType

expTypeNumeric :: DataTypeMap -> TypeMap -> Exp -> Exp -> Either String ExpType
expTypeNumeric dm em e1 e2 = do
  et1 <- expType dm em e1
  guardE (Right et1 == expType dm em e2) $ varTypeError "numeric types" dm em e1 e2
  guardE (isNumericType et1) $ "Not a numeric type: " ++ showExpType dm em e1
  return et1

compTypeError :: DataTypeMap -> TypeMap -> Exp -> Exp -> String
compTypeError = varTypeError "comparison"

varTypeError :: String -> DataTypeMap -> TypeMap -> Exp -> Exp -> String
varTypeError msg dm tm e1 e2 =
  let et1 = showExpType dm tm e1
      et2 = showExpType dm tm e2
  in typeError msg et1 et2

typeError :: String -> String -> String -> String
typeError msg e1 e2 =
  "Type error: " ++ msg ++ ": Expression has type " ++ show e1 ++ " - expected " ++ show e2

showExpType :: DataTypeMap -> TypeMap -> Exp -> String
showExpType dm tm e1 = either (const "<unknown>") show (expType dm tm e1)

expType :: DataTypeMap -> TypeMap -> Exp -> Either String ExpType
expType dm em (Plus e1 e2) = expTypeNumeric dm em e1 e2
expType dm em (Minus e1 e2) = expTypeNumeric dm em e1 e2
expType dm em (Times e1 e2) = expTypeNumeric dm em e1 e2
expType dm em (Div e1 e2) = expTypeNumeric dm em e1 e2
expType dm em (CmpEq e1 e2) = guardE (on (==) (expType dm em) e1 e2) (compTypeError dm em e1 e2) >> Right TypeBool
expType dm em (CmpLt e1 e2) = guardE (on (==) (expType dm em) e1 e2) (compTypeError dm em e1 e2) >> Right TypeBool
expType dm em (CmpLe e1 e2) = guardE (on (==) (expType dm em) e1 e2) (compTypeError dm em e1 e2) >> Right TypeBool
expType _  _  (Int _) = Right TypeInt
expType dm em (FunApp n params) = case M.lookup n em of
                                 Nothing -> Left $ "Unknown variable: `" ++ n ++ "'"
                                 Just (TypeFun expectedParams) ->
                                   let numparams = length params
                                       numExpectedParams = length expectedParams - 1
                                   in if numparams == numExpectedParams
                                        then do
                                          partypes <- mapM (expType dm em) params
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
expType dm em (DataCons n params) = do
  mapM_ (expType dm em) params
  case M.lookup n em of
    Nothing ->
      if M.member n dm then Right (CustomType n) else Left $ "Unknown data constructor: " ++ n
    Just r  -> Right r
expType dm em (Brack e) = expType dm em e
expType dm em (Negate e) = do
  et <- expType dm em e
  guardE (isNumericType et) "Type error on numeric type comparison"
  return et
expType dm em (IfThenElse e1 e2 e3) = do
  et1 <- expType dm em e1
  et2 <- expType dm em e2
  et3 <- expType dm em e3
  guardE (et1 == TypeBool) $ typeError "`if' predicate" "Bool" $ showExpType dm em e1
  guardE (et2 == et3) $ typeError "`if' branches" (showExpType dm em e2) (showExpType dm em e3)
  return et2

guardE :: Bool -> String -> Either String ()
guardE True  _ = return ()
guardE False e = fail e


