module Asm(generateAssembly)
where

import Data.List
import qualified Data.Map as M
import Control.Applicative
import Control.Monad
import Control.Monad.State
import Control.Monad.Error()

import Stau
import StauTypes

type FunctionMap = M.Map String FunctionInfo

data FunctionInfo = FunctionInfo {
    funNumber          :: Int
  , funStackHeightDiff :: Int
 }

data Opcode = OpInt Int
            | OpAdd
            | OpSub
            | OpMul
            | OpDiv
            | OpLT
            | OpLE
            | OpEQ
            | OpDup
            | OpNop
            | OpDrop
            | OpSwap
            | OpFunDef Int Int
            | OpFunEnd
            | OpRet0
            | OpRet1
            | OpFunCall FunctionInfo
            | OpBr Int
            | OpBrNz Int
            | OpLoad Int
            | OpNew
            | OpRStore
            | OpRLoad
            | OpPFunCall Int
            | OpPFunID Int

addLF :: String -> String
addLF = (++ "\n")

instance Show Opcode where
  show (OpInt i)     = addLF $ show i
  show OpAdd         = addLF $ "ADD"
  show OpSub         = addLF $ "SUB"
  show OpMul         = addLF $ "MUL"
  show OpDiv         = addLF $ "DIV"
  show OpLT          = addLF $ "LT"
  show OpLE          = addLF $ "LE"
  show OpEQ          = addLF $ "EQ"
  show OpNop         = addLF $ "NOP"
  show OpDup         = addLF $ "DUP"
  show OpDrop        = addLF $ "DROP"
  show OpSwap        = addLF $ "SWAP"
  show (OpFunDef i j) = addLF $ "FUNDEF " ++ show i ++ " " ++ show j
  show OpFunEnd      = addLF $ "FUNEND"
  show OpRet0        = addLF $ "RET0"
  show OpRet1        = addLF $ "RET1"
  show (OpFunCall f) = addLF $ "FUNCALL " ++ show (funNumber f)
  show (OpBr i)      = addLF $ "BR " ++ show i
  show (OpBrNz i)    = addLF $ "BRNZ " ++ show i
  show (OpLoad i)    = addLF $ "LOAD " ++ show i
  show OpNew         = addLF $ "NEW"
  show OpRStore      = addLF $ "RSTORE"
  show OpRLoad       = addLF $ "RLOAD"
  show (OpPFunCall _) = addLF $ "FUNPCALL"
  show (OpPFunID i)  = addLF $ 'f' : show i

generateAssembly :: Module -> [Opcode]
generateAssembly ast = evalState (generateAssembly' fns) $ CompileState 0 0 0 0 fm 
                              undefined dsz cmap
  where fm = createFunctionMap fns
        fns = moduleFunctions ast
        dsz = createDataSizeMap $ moduleDataDecls ast ++ preludeTypes
        cmap = datatypeMap $ moduleDataDecls ast ++ preludeTypes

initialVarMap :: VariableMap
initialVarMap = valueMapToVariableMap preludeVariables

valueMapToVariableMap :: ValueMap -> VariableMap
valueMapToVariableMap = M.map valueToVariable

valueToVariable :: Value -> AsmVariable
valueToVariable (StackValue i _) = StackVariable i
valueToVariable (ExpValue e _)   = ExpVariable e

createDataSizeMap :: [DataDecl] -> M.Map String Int
createDataSizeMap ds =
  M.fromList $ zip (map dataDeclName ds) (map dataDeclSize ds)

dataDeclSize :: DataDecl -> Int
dataDeclSize dt =
  let maxTypeSize = maximum (map ((*4) . length . dataFieldTypes) (dataConstructors dt))
      enumAdd     = if hasMultipleConstructors dt then 4 else 0
  in maxTypeSize + enumAdd

type DataTypeMap = M.Map String (DataDecl, Constructor)

datatypeMap :: [DataDecl] -> DataTypeMap
datatypeMap decls = 
           buildMapsFrom (constructorName . snd)
               (map (\(a, b) -> (a, b)) . pair $ assocsBy dataConstructors decls)

hasMultipleConstructors :: DataDecl -> Bool
hasMultipleConstructors dt =
  case dataConstructors dt of
    []  -> False
    [_] -> False
    _   -> True

generateAssembly' :: [Function] -> State CompileState [Opcode]
generateAssembly' fns =
  concat `liftM` forM fns genFunctionAsm

data CompileState = CompileState {
    currPos :: Int
  , stackHeight :: Int
  , numVars :: Int
  , minVars :: Int
  , functions :: FunctionMap
  , variables :: VariableMap
  , datasizes :: M.Map String Int
  , dtmap :: DataTypeMap
  }

type VariableMap = M.Map String AsmVariable

data AsmVariable = StackVariable Int
                 | ExpVariable Exp
                 | RefVariable [(Int, Int)]

getFunParamVars :: Function -> [(String, AsmVariable)]
getFunParamVars f =
  getParamVars $ getFunArgs f

getParamVars :: [ParamDecl] -> [(String, AsmVariable)]
getParamVars ps = concatMap (uncurry getParamVar) (zip [1..] ps)

getParamVar :: Int -> ParamDecl -> [(String, AsmVariable)]
getParamVar num (VariableParam name)    = [(name, StackVariable (-num))]
getParamVar num (ConstructorParam _ ps) = concatMap (uncurry (getConsParamVar num)) (zip [0, 4..] ps)
getParamVar _   WildcardParam           = []

getConsParamVar :: Int -> Int -> ParamDecl -> [(String, AsmVariable)]
getConsParamVar stp addr (VariableParam name)    = [(name, RefVariable [(-stp, addr)])]
getConsParamVar stp _    (ConstructorParam _ ps) = concatMap (uncurry (getConsParamVar stp)) (zip [0, 4..] ps)
getConsParamVar _   _    WildcardParam           = []

genFunctionAsm :: Function -> State CompileState [Opcode]
genFunctionAsm f = do
  fm <- functions <$> get
  let fi       = fm M.! getFunName f
      numArgs  = length $ getFunArgs f
      paramMap = M.fromList (getFunParamVars f)
  modify $ \c -> c{numVars = numArgs, minVars = numArgs,
                   variables = initialVarMap `M.union` paramMap}
  fd <- addOp $ OpFunDef (funNumber fi) (-(funStackHeightDiff fi))
  fds <- genExprAsm (getFunExp f)
  fe <- addOp $ if numArgs == 0 then OpFunEnd else OpRet1
  modify $ \c -> c{stackHeight = 0}
  return $ fd : fds ++ [fe]

genArithAsm :: Opcode -> Exp -> Exp -> State CompileState [Opcode]
genArithAsm op e1 e2 = do
  a1 <- genExprAsm e1
  a2 <- genExprAsm e2
  rmVar
  opc <- addOp op
  return $ a1 ++ a2 ++ [opc]

genExprAsm :: Exp -> State CompileState [Opcode]
genExprAsm (Plus e1 e2)  = genArithAsm OpAdd e1 e2
genExprAsm (Minus e1 e2) = genArithAsm OpSub e1 e2
genExprAsm (Times e1 e2) = genArithAsm OpMul e1 e2
genExprAsm (Div e1 e2)   = genArithAsm OpDiv e1 e2
genExprAsm (Int i)       = addVar >> sequence [addOp $ OpInt i]
genExprAsm (Brack e)     = genExprAsm e

genExprAsm (IfThenElse e1 e2 e3) = do
  o1 <- genExprAsm e1
  rmVar
  _  <- addOp $ OpBrNz 0 -- placeholder
  elseDrops <- addDrops
  elseBr <- genExprAsm e3
  _  <- addOp $ OpBr 0   -- placeholder
  l2 <- currPos <$> get
  thenBr <- genExprAsm e2
  l3 <- currPos <$> get
  let br1 = [OpBrNz l2]
      br2 = [OpBr l3]
  return $ concat [o1, br1, elseDrops, elseBr, br2, thenBr]

genExprAsm (Variable vn) = do
  fm <- functions <$> get
  vars <- variables <$> get
  case M.lookup vn vars of
    Nothing -> 
      case M.lookup vn fm of
        Nothing -> icError $ "Variable \"" ++ vn ++ "\" not defined"
        Just f  -> sequence [addOp . OpPFunID $ funNumber f]
    Just v  -> genVarAsm v

genExprAsm (FunApp fn eps) = do
  fm <- functions <$> get
  if null eps
    then do
      vars <- variables <$> get
      case M.lookup fn vars of
        Nothing -> 
          case M.lookup fn fm of
            Nothing -> icError $ "Variable \"" ++ fn ++ "\" not defined"
            Just f  -> sequence [addOp $ OpFunCall f]
        Just v  -> genVarAsm v
    else do
      params <- concat <$> mapM genExprAsm (reverse eps)
      fcall <- case M.lookup fn fm of
                 Nothing -> do
                   vars <- variables <$> get
                   case M.lookup fn vars of
                     Nothing -> icError $ "Function \"" ++ fn ++ "\" not defined"
                     Just v  -> liftM2 (++) (genVarAsm v) (fmap (:[]) (addOp $ OpPFunCall numParams))
                       where numParams = length eps
                 Just f  -> sequence [addOp $ OpFunCall f]
      return $ params ++ fcall

genExprAsm (DataCons consname exps) = do
  consMap <- dtmap <$> get
  case M.lookup consname consMap of
    Nothing -> do
      vars <- variables <$> get
      case M.lookup consname vars of
        Nothing -> icError $ "undeclared data constructor " ++ consname
        Just v  -> genVarAsm v
    Just (dtt, _) -> do
      dataszs <- datasizes <$> get
      case M.lookup (dataDeclName dtt) dataszs of
        Nothing       -> icError $ "unknown data type " ++ dataDeclName dtt
        Just datasize -> do
          osz <- if datasize == 4
            then return []
            else sequence [addOp $ OpInt datasize, addOp OpNew]
          consasm <-
            if not (hasMultipleConstructors dtt)
              then return []
              else
                case findIndex (\c -> constructorName c == consname) (dataConstructors dtt) of
                  Nothing -> icError $ "unknown data constructor " ++ consname
                  Just i  ->
                    if datasize == 4
                      then sequence [addOp $ OpInt i]
                      else do
                        odup <- addOp OpDup
                        cidx <- addOp $ OpInt 0
                        ea   <- addOp $ OpInt i
                        cst  <- addOp OpRStore
                        return [odup, cidx, ea, cst]
          params <- forM (zip [0..] exps) $ \(i, e) -> do
            odup <- addOp OpDup
            oidx <- addOp $ OpInt $ paramIndexToAddr i (hasMultipleConstructors dtt) * 4
            ea <- genExprAsm e
            ost <- addOp OpRStore
            return $ [odup, oidx] ++ ea ++ [ost]
          return $ osz ++ consasm ++ concat params

genExprAsm (CmpLt e1 e2) = genArithAsm OpLT e1 e2
genExprAsm (CmpLe e1 e2) = genArithAsm OpLE e1 e2
genExprAsm (CmpEq e1 e2) = genArithAsm OpEQ e1 e2
genExprAsm (Negate (Int i)) = addVar >> sequence [addOp $ OpInt (-i)]
genExprAsm (Negate n)       = genArithAsm OpMul (Int (-1)) n
genExprAsm (CaseOf e1 ps)   = do
  ea <- genExprAsm e1
  pa <- caseExprAsm ps
  return $ ea ++ pa

caseExprAsm :: [CasePattern] -> State CompileState [Opcode]
caseExprAsm [] = liftM2 (:) (addOp OpDrop) patternMatchErrorAsm
caseExprAsm (Case WildcardParam ex:_) = liftM2 (:) (addOp OpDrop) $ genExprAsm ex
caseExprAsm (Case (VariableParam n) ex:_) = do
  -- this is probably OK scope-wise..?
  addLocalVar n Nothing
  liftM2 (:) (addOp OpDrop) $ genExprAsm ex
caseExprAsm (Case (ConstructorParam sn ps) ex:cases) = do
  -- TODO: save match result to a temporary variable
  match <- matchAsm sn ps
  rmVar
  _  <- addOp $ OpBrNz 0 -- placeholder
  elseBr <- caseExprAsm cases
  _  <- addOp $ OpBr 0   -- placeholder
  l2 <- currPos <$> get
  thenDrops <- addDrops
  thenBr <- liftM2 (++) (genExprAsm ex) $ sequence [addOp OpSwap, addOp OpDrop]
  l3 <- currPos <$> get
  let br1 = [OpBrNz l2]
      br2 = [OpBr l3]
  return $ concat [match, br1, elseBr, br2, thenDrops, thenBr]

-- sh +1: [cons enum] => [cons enum, branch or not]
matchAsm :: String -> [ParamDecl] -> State CompileState [Opcode]
matchAsm sn ps = do
  da <- addOp OpDup
  consMap <- dtmap <$> get
  -- cncmp and pscmp mustn't change stack height
  (dtt, cncmp) <- do
    (dt, cn) <- fetch sn consMap "constructor"
    case getConstructorEnum dt cn of
      Nothing -> return (dt, [])
      Just n  -> do
        dataszs <- datasizes <$> get
        datasize <- fetch (dataDeclName dt) dataszs "data type"
        getcons <- if datasize == 4
                        then return []
                        else sequence [addOp $ OpInt 0, addOp OpRLoad]
        cmpcons <- sequence $ [addOp $ OpInt n, addOp OpEQ]
        return (dt, getcons ++ cmpcons)
  pscmp <- concat <$> mapM (uncurry (matchParam $ hasMultipleConstructors dtt)) (zip [0..] ps)
  return $ [da] ++ cncmp ++ pscmp

fetch :: String -> M.Map String a -> String -> State CompileState a
fetch key m tp =
  case M.lookup key m of
    Nothing -> icError $ "Unknown " ++ tp ++ ": `" ++ key ++ "'"
    Just v  -> return v

matchParam :: Bool -> Int -> ParamDecl -> State CompileState [Opcode]
matchParam _ _ WildcardParam     = return []
matchParam m i (VariableParam n) = addLocalVar n (Just (i, m)) >> return []
matchParam _ _ (ConstructorParam sn ps) = matchAsm sn ps

addLocalVar :: String -> Maybe (Int, Bool) -> State CompileState ()
addLocalVar n Nothing = do
  sh <- stackHeight <$> get
  modify $ \c -> c{variables = variables c `M.union` M.fromList [(n, StackVariable (sh - 1))]}
addLocalVar n (Just (i, t)) = do
  sh <- stackHeight <$> get
  modify $ \c -> c{variables = variables c `M.union` M.fromList [(n, RefVariable [(sh - 2, 4 * paramIndexToAddr i t)])]}

getLocalVar :: String -> State CompileState [Opcode]
getLocalVar str = do
  vars <- variables <$> get
  genVarAsm $ vars M.! str

getConstructorEnum :: DataDecl -> Constructor -> Maybe Int
getConstructorEnum dt cn =
  let conss = dataConstructors dt
      consnames = map constructorName conss
  in if singleton conss
       then Nothing
       else findIndex (== constructorName cn) consnames

-- | Params: index and whether the data type has multiple constructors
paramIndexToAddr :: Int -> Bool -> Int
paramIndexToAddr i True  = i + 1
paramIndexToAddr i False = i

singleton :: [a] -> Bool
singleton [_] = True
singleton _   = False

icError :: String -> a
icError msg = error $ "Internal compiler error on code generation: " ++ msg

patternMatchErrorAsm :: State CompileState [Opcode]
patternMatchErrorAsm = fmap return (addOp (OpInt 0)) -- TODO: implement exit() in VM

genVarAsm :: AsmVariable -> State CompileState [Opcode]
genVarAsm (StackVariable v) =
  fmap (:[]) $ addOp (OpLoad v)
genVarAsm (ExpVariable e)   = genExprAsm e
genVarAsm (RefVariable rs)  =
  concat <$> forM rs (\(stp, addr) -> do
    st <- addOp $ OpLoad stp
    ad <- addOp $ OpInt addr
    rl <- addOp $ OpRLoad
    return [st, ad, rl])

addVar, rmVar :: State CompileState ()
addVar = addVars 1
rmVar  = rmVars 1

addVars, rmVars :: Int -> State CompileState ()
addVars i = modify $ \c -> c{numVars = numVars c + i}
rmVars  i = modify $ \c -> c{numVars = numVars c - i}

addDrops :: State CompileState [Opcode]
addDrops = do
  numDrops <- numVars <$> get
  drops <- forM [2..numDrops] $ \_ -> addOp OpDrop
  modify $ \c -> c{numVars = minVars c}
  return drops

addOp :: Opcode -> State CompileState Opcode
addOp op = do
  modify $ \c -> c{currPos = currPos c + opLength op,
                   stackHeight = stackHeight c + stackHeightDiff op}
  return op

opLength :: Opcode -> Int
opLength (OpInt _)     = 5
opLength OpAdd         = 1
opLength OpSub         = 1
opLength OpMul         = 1
opLength OpDiv         = 1
opLength OpLT          = 1
opLength OpLE          = 1
opLength OpEQ          = 1
opLength OpDup         = 1
opLength OpDrop        = 1
opLength OpNop         = 1
opLength OpSwap        = 1
opLength (OpFunDef _ _) = 5
opLength OpFunEnd      = 1
opLength OpRet0        = 1
opLength OpRet1        = 1
opLength (OpFunCall _) = 5
opLength (OpBr _)      = 5
opLength (OpBrNz _)    = 5
opLength (OpLoad _)    = 5
opLength OpNew         = 1
opLength OpRStore      = 1
opLength OpRLoad       = 1
opLength (OpPFunCall _) = 1
opLength (OpPFunID _)  = 5

stackHeightDiff :: Opcode -> Int
stackHeightDiff (OpInt _)     = 1
stackHeightDiff OpAdd         = -1
stackHeightDiff OpSub         = -1
stackHeightDiff OpMul         = -1
stackHeightDiff OpDiv         = -1
stackHeightDiff OpLT          = -1
stackHeightDiff OpLE          = -1
stackHeightDiff OpEQ          = -1
stackHeightDiff OpDup         = 1
stackHeightDiff OpDrop        = -1
stackHeightDiff OpNop         = 0
stackHeightDiff OpSwap        = 0
stackHeightDiff (OpFunDef _ _) = 0 -- not used
stackHeightDiff OpFunEnd      = 0 -- not used
stackHeightDiff OpRet0        = 0 -- not used
stackHeightDiff OpRet1        = 0 -- not used
stackHeightDiff (OpFunCall f) = funStackHeightDiff f
stackHeightDiff (OpBr _)      = 0
stackHeightDiff (OpBrNz _)    = -1
stackHeightDiff (OpLoad _)    = 1
stackHeightDiff OpNew         = 0
stackHeightDiff OpRStore      = -2
stackHeightDiff OpRLoad       = -1
stackHeightDiff (OpPFunCall p) = getFunStackHeightDiff p
stackHeightDiff (OpPFunID _)  = 1

createFunctionMap :: [Function] -> FunctionMap
createFunctionMap = M.adjust (const $ FunctionInfo 1 0) "main" . fst . foldl' go (M.empty, 2)
  where go (mp, nr) f = (M.insertWithKey (\k _ _ -> icError $ "Function \"" ++ k ++ "\" already defined.")
                               (getFunName f) (getFunInfo nr f) mp, succ nr)

getFunInfo :: Int -> Function -> FunctionInfo
getFunInfo nr f = FunctionInfo nr (getFunStackHeightDiff $ length $ getFunArgs f)

getFunStackHeightDiff :: Int -> Int
getFunStackHeightDiff numArgs = -(max 0 (numArgs - 1))

