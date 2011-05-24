module TypeCheck(typeCheck,
  Context, VariableName(..), Type(..), TypeVariable(..))
where

import Data.Char
import Data.Either
import Data.List
import Data.Maybe
import qualified Data.Map as M
import qualified Data.Set as S
import Control.Applicative
import Control.Monad
import Control.Monad.State
import Control.Monad.Error

import Stau
import StauTypes

type TypeCheckMonad = Either String

newtype VariableName = VariableName { unVariableName :: String }
  deriving (Show, Eq, Ord)

data Type = TypeVariable TypeVariable
          | IntType
          | BoolType
          | Custom String
          | FunType [Type] Type
  deriving (Eq, Ord)

instance Show Type where
  show (TypeVariable (TV f i n)) = f ++ "_" ++ i : show n
  show IntType  = "Int"
  show BoolType = "Bool"
  show (Custom n) = n
  show (FunType es e) = "(" ++ intercalate " -> " (map show (es ++ [e])) ++ ")"

type Context = M.Map VariableName Type

type Constraint = M.Map TypeVariable Type 

data TypeVariable = TV String Char Int 
  deriving (Show, Eq, Ord)

data FunCheckState = FunCheckState {
    typevariable   :: TypeVariable
  , context        :: Context
  , constructormap :: ConstructorMap
  , constraints    :: Constraint
  }

-- helper functions
type ConstructorMap = M.Map String (Type, Constructor)

constructorMap :: [DataDecl] -> ConstructorMap
constructorMap decls = 
           buildMapsFrom (constructorName . snd)
               (map (\(a, b) -> (Custom (dataDeclName a), b)) . pair $ assocsBy dataConstructors decls)

tcError :: String -> String -> TypeCheckMonad a
tcError name msg = throwError $ msg ++ ": `" ++ name ++ "'"

createMapFrom :: (Ord k) => (a -> k) -> (a -> b) -> [a] -> M.Map k b
createMapFrom f g xs = M.fromList $ zip (map f xs) (map g xs)

-- entry
typeCheck :: Module -> TypeCheckMonad Context
typeCheck m = do
  globalContext <- initialModuleContext m
  -- TODO: warn if user overrides prelude
  -- TODO: check consmap validity
  let consmap     = constructorMap (moduleDataDecls m)
      consprelude = M.fromList [("True", (BoolType, Constructor "True" [])),
                                ("False", (BoolType, Constructor "False" []))]
      consmap' = M.union consprelude consmap
  context <$> (flip execStateT (FunCheckState undefined globalContext consmap' M.empty) $ do
                forM_ (moduleFunctions m) $ \fun -> do
                  modify $ \s -> s{typevariable = TV (getFunName fun) 'a' 1}
                  ft <- runTypecheck fun
                  addConstraint (TV (getFunName fun) 'a' 0) ft
                unify)

unifyType :: (TypeVariable, Type) -> StateT FunCheckState TypeCheckMonad ()
unifyType (tv, t) = do
      ctxt <- context <$> get
      forM_ (M.toList ctxt) (specifyType (tv, t))

specifyType :: (TypeVariable, Type) -> (VariableName, Type) -> StateT FunCheckState TypeCheckMonad ()
specifyType (tv, t) (vn, t'@(TypeVariable tv2)) =
  when (tv2 == tv) $ do
    nt <- unifyTypes t t'
    modify $ \s -> s{context = M.insert vn nt (context s)}
specifyType (tv, t) (vn, (FunType ps r)) = do
  r' <- case r of
          TypeVariable tv2 ->
            if (tv2 == tv) then unifyTypes t r else return r
          _ -> return r
  ps' <- forM ps $ \p -> case p of
           TypeVariable tv2 ->
             if tv2 == tv
               then unifyTypes t p
               else return p
           _ -> return p
  modify $ \s -> s{context = M.insert vn (FunType ps' r') (context s)}
specifyType _ _ = return ()

unifyTypes :: Type -> Type -> StateT FunCheckState TypeCheckMonad Type
unifyTypes t1 (TypeVariable _) = return t1
unifyTypes (TypeVariable _) t2 = return t2
unifyTypes (FunType p1 r1) (FunType p2 r2) = do
  r <- unifyTypes r1 r2
  ps <- zipWithM unifyTypes p1 p2
  return $ FunType ps r
unifyTypes t1 t2
  | t1 /= t2  = typeError "" t1 t2
  | otherwise = return t2

unify :: StateT FunCheckState TypeCheckMonad ()
unify = do
  cons <- constraints <$> get
  mapM_ unifyType (M.toList cons)

typeError :: String -> Type -> Type -> StateT FunCheckState TypeCheckMonad a
typeError ""   t1 t2 = throwError $ "expected `" ++ show t2 ++ "', inferred `" ++ show t1 ++ "'"
typeError name t1 t2 = throwError $ "on definition of `" ++ name ++ "': expected `" ++ show t2 ++ "', inferred `" ++ show t1 ++ "'"

initialModuleContext :: Module -> TypeCheckMonad Context
initialModuleContext m = do
  funs <- createMapFrom fst snd <$> mapM initialContext (moduleFunctions m)
  sigs <- createMapFrom fst snd <$> mapM initialSigContext (moduleSignatures m)
  -- TODO: improve this error message
  when (not . S.null $ M.keysSet sigs S.\\ M.keysSet funs) $
    throwError $ "Signature without function declaration"
  return $ M.union sigs funs

initialFunTV :: Function -> TypeVariable
initialFunTV fun = TV (getFunName fun) 'a' 0

initialContext :: Function -> TypeCheckMonad (VariableName, Type)
initialContext fun = return (VariableName (getFunName fun), TypeVariable $ initialFunTV fun)

initialSigContext :: FunSig -> TypeCheckMonad (VariableName, Type)
initialSigContext funsig = do
  types <- mapM (sigTypeToType (getFunSigName funsig)) (getFunSigTypes funsig)
  ctxttype <- case types of
                []  -> tcError (getFunSigName funsig) "Invalid type signature"
                [x] -> return x
                ts  -> return $ FunType (init ts) (last ts)
  return (VariableName (getFunSigName funsig), ctxttype)

-- TODO: function types
sigTypeToType :: String -> String -> TypeCheckMonad Type
sigTypeToType _ "Int"  = return $ IntType
sigTypeToType _ "Bool" = return $ BoolType
sigTypeToType _ []     = throwError $ "Invalid type signature"
sigTypeToType fn xs | isLower (head xs) = return $ TypeVariable $ TV (fn ++ "_" ++ xs) (head xs) 0
                    | otherwise         = return $ Custom xs

runTypecheck :: Function -> StateT FunCheckState TypeCheckMonad Type
runTypecheck fun = do
  prevctxt <- context <$> get
  paramtypes <- mapM paramType (getFunArgs fun)
  funt <- expType (getFunExp fun)
  paramtypes' <- zipWithM fetchParamType (getFunArgs fun) paramtypes
  updContext prevctxt
  funType paramtypes' funt

updContext :: Context -> StateT FunCheckState TypeCheckMonad ()
updContext ctxt = do
  s <- get
  put (s{context = ctxt})

fetchParamType :: ParamDecl -> Type -> StateT FunCheckState TypeCheckMonad Type
fetchParamType (VariableParam n) def = do
  ctxt <- context <$> get
  case M.lookup (VariableName n) ctxt of
    Nothing -> return def
    Just t' -> return t'
fetchParamType _ def = return def

funType :: [Type] -> Type -> StateT FunCheckState TypeCheckMonad Type
funType paramtypes t =
  return $ mkFunType paramtypes t

mkFunType :: [Type] -> Type -> Type
mkFunType [] t = t
mkFunType ps t = FunType ps t

paramType :: ParamDecl -> StateT FunCheckState TypeCheckMonad Type
paramType (VariableParam n) = do
  ntv <- nextTypeVariable
  addToContext n (TypeVariable ntv)

paramType (ConstructorParam sn params) = do
  consmap <- constructormap <$> get
  case M.lookup sn consmap of
    Nothing -> throwError $ "Not in scope: `" ++ sn ++ "'"
    Just (dt, ct) -> do
      types <- mapM typeNameToType (dataFieldTypes ct)
      ptypes <- mapM paramType params
      when (length types /= length ptypes) $ 
        throwError $ "Invalid data fields for `" ++ sn ++ "'"
      zipWithM_ checkType types ptypes
      return dt

paramType WildcardParam = nextTypeVariable >>= return . TypeVariable

getParamType :: ConstructorMap -> ParamDecl -> Either String (Maybe Type)
getParamType _       WildcardParam = Right Nothing
getParamType _       (VariableParam _) = Right Nothing
getParamType consmap (ConstructorParam sn params) =
  case M.lookup sn consmap of
    Nothing       -> Left $ "Not in scope: `" ++ sn ++ "'"
    Just (dt, ct) -> if length params == length (dataFieldTypes ct)
                       then do
                         mapM_ (getParamType consmap) params
                         return $ Just dt
                       else Left $ "Invalid parameters for constructor `" ++ constructorName ct ++ "'"

nextTypeVariable :: (Monad m) => StateT FunCheckState m TypeVariable
nextTypeVariable = do
  tv <- typevariable <$> get
  modify $ \f -> f{typevariable = incTypeVariable (typevariable f)}
  return tv

incTypeVariable :: TypeVariable -> TypeVariable
incTypeVariable (TV fn 't' n) = TV fn 't' (succ n)
incTypeVariable (TV fn c   n) = TV fn (succ c) n

addToContext :: String -> Type -> StateT FunCheckState TypeCheckMonad Type
addToContext tv t = do
  ctxt <- context <$> get
  case M.lookup (VariableName tv) ctxt of
    Nothing -> do
      modify $ \s -> s{context = M.insert (VariableName tv) t ctxt}
      return t
    -- TODO: allow shadowing
    Just _  -> throwError $ "Shadowing variable `" ++ tv ++ "'"

-- No check for overriding for now.
addConstraint :: TypeVariable -> Type -> StateT FunCheckState TypeCheckMonad ()
addConstraint tv t = do
  cons <- constraints <$> get
  case M.lookup tv cons of
    Just t' -> if TypeVariable tv == t'
                 then return ()
                 else checkType t t'
    Nothing -> do
      modify $ \s -> s{constraints = M.insert tv t (constraints s)}

checkType :: Type -> Type -> StateT FunCheckState TypeCheckMonad ()
checkType (TypeVariable tv) t2 = addConstraint tv t2
checkType t1 (TypeVariable tv) = addConstraint tv t1
checkType t1@(FunType p1 r1) t2@(FunType p2 r2) = do
  zipWithM_ checkType p1 p2
  when (length p1 /= length p2) $ typeError "" t1 t2
  checkType r1 r2
checkType t1 t2 | t1 == t2  = return ()
                | otherwise = typeError "" t1 t2

expTypeNum :: Exp -> Exp -> StateT FunCheckState TypeCheckMonad Type
expTypeNum e1 e2 = do
  et1 <- expType e1
  et2 <- expType e2
  checkType et1 IntType
  checkType et2 IntType
  return IntType

expTypeComp :: Exp -> Exp -> StateT FunCheckState TypeCheckMonad Type
expTypeComp e1 e2 = do
  et1 <- expType e1
  et2 <- expType e2
  checkType et1 IntType
  checkType et2 IntType
  return BoolType

expType :: Exp -> StateT FunCheckState TypeCheckMonad Type
expType (Plus e1 e2) = expTypeNum e1 e2
expType (Minus e1 e2) = expTypeNum e1 e2
expType (Times e1 e2) = expTypeNum e1 e2
expType (Div e1 e2) = expTypeComp e1 e2
expType (CmpEq e1 e2) = expTypeComp e1 e2
expType (CmpLt e1 e2) = expTypeComp e1 e2
expType (CmpLe e1 e2) = expTypeComp e1 e2
expType (Int _) = return IntType

expType (Variable n) = do
  ctxt <- context <$> get
  case M.lookup (VariableName n) ctxt of
    Nothing -> throwError $ "Not in scope: `" ++ n ++ "'"
    Just t  -> return t

expType (FunApp n []) = do
  ctxt <- context <$> get
  case M.lookup (VariableName n) ctxt of
    Nothing -> throwError $ "Not in scope: `" ++ n ++ "'"
    Just t  -> return t

expType (FunApp n es) = do
  etypes <- mapM expType es
  ctxt <- context <$> get
  rettype <- nextTypeVariable 
  let funtype = FunType etypes (TypeVariable rettype)
  case M.lookup (VariableName n) ctxt of
    Nothing -> throwError $ "Not in scope: `" ++ n ++ "'"
    Just (TypeVariable p) -> do
      -- TODO: add constraints to function type?
      addConstraint p funtype
      return (TypeVariable rettype)
    Just t@(FunType params ret) -> do
      zipWithM_ checkType etypes params
      -- TODO: currying?
      when (length params /= length es) $ do
        typeError n funtype t
      return ret
    Just t -> typeError n funtype t

expType (DataCons n es) = do
  consmap <- constructormap <$> get
  case M.lookup n consmap of
    Nothing -> throwError $ "Not in scope: `" ++ n ++ "'"
    Just (dt, ct) -> do
      types <- mapM typeNameToType (dataFieldTypes ct)
      etypes <- mapM expType es
      when (length types /= length etypes) $ 
        throwError $ "Invalid data fields for `" ++ n ++ "'"
      zipWithM_ checkType types etypes
      return dt

expType (Brack e) = expType e
expType (Negate e) = expType (Times (Int (-1)) e)

expType (IfThenElse e1 e2 e3) = do
  et1 <- expType e1
  et2 <- expType e2
  et3 <- expType e3
  checkType et1 BoolType
  checkType et2 et3
  return et2

expType (CaseOf e patterns) = do
  consmap <- constructormap <$> get
  let mctypes = map (getParamType consmap) (map caseParams patterns)
  case lefts mctypes of
    (p:_) -> throwError p
    []    -> do
      case the (catMaybes $ rights mctypes) of
        Nothing -> throwError $ "case pattern: not all cases have the same type"
        Just t  -> do
          et <- expType e
          checkType t et
          exptypes <- mapM (\(Case p ex) -> withContext $ paramType p >> expType ex) patterns
          matchAll exptypes
          case exptypes of
            []     -> throwError "Error: case of with no cases"
            (t':_) -> return t'

withContext :: StateT FunCheckState TypeCheckMonad a -> StateT FunCheckState TypeCheckMonad a
withContext a = do
  ctxt1 <- context <$> get
  res <- a
  s' <- get
  put $ s'{context=ctxt1}
  return res

-- TODO: function types
typeNameToType :: String -> StateT FunCheckState TypeCheckMonad Type
typeNameToType "Int"  = return IntType
typeNameToType "Bool" = return BoolType
typeNameToType n = return $ Custom n

matchAll :: [Type] -> StateT FunCheckState TypeCheckMonad ()
matchAll [] = return ()
matchAll (t:ts) = matchAll' t ts
  where matchAll' _ [] = return ()
        matchAll' tr@(TypeVariable tv) (n:ns) = do
          addConstraint tv n
          matchAll' tr ns
        matchAll' tr (n:ns) = do
          checkType tr n
          matchAll' tr ns

the :: (Eq a) => [a] -> Maybe a
the [] = Nothing
the (x:xs) | all (x==) xs = Just x
           | otherwise    = Nothing

