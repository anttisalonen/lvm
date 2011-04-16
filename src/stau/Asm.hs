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

type FunctionMap = M.Map String Int

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
            | OpFunCall Int
            | OpBr Int
            | OpBrNz Int
            | OpLoad Int
            | OpNew
            | OpRStore
            | OpRLoad

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
  show (OpFunCall i) = addLF $ "FUNCALL " ++ show i
  show (OpBr i)      = addLF $ "BR " ++ show i
  show (OpBrNz i)    = addLF $ "BRNZ " ++ show i
  show (OpLoad i)    = addLF $ "LOAD " ++ show i
  show OpNew         = addLF $ "NEW"
  show OpRStore      = addLF $ "RSTORE"
  show OpRLoad       = addLF $ "RLOAD"

generateAssembly :: Module -> [Opcode]
generateAssembly ast = evalState (generateAssembly' fns) $ CompileState 0 0 0 fm 
                              (valueMapToVariableMap preludeVariables) dsz cmap
  where fm = createFunctionMap fns
        fns = moduleFunctions ast
        dsz = createDataSizeMap (moduleDataDecls ast)
        cmap = buildDataTypeMap ast

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
  , numVars :: Int
  , minVars :: Int
  , functions :: FunctionMap
  , variables :: VariableMap
  , datasizes :: M.Map String Int
  , constructorMap :: DataTypeMap
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
getParamVar num (VariableParam name)    = [(name, StackVariable num)]
getParamVar num (ConstructorParam _ ps) = concatMap (uncurry (getConsParamVar num)) (zip [0, 4..] ps)

getConsParamVar :: Int -> Int -> ParamDecl -> [(String, AsmVariable)]
getConsParamVar stp addr (VariableParam name)    = [(name, RefVariable [(stp, addr)])]
getConsParamVar stp _    (ConstructorParam _ ps) = concatMap (uncurry (getConsParamVar stp)) (zip [0, 4..] ps)

genFunctionAsm :: Function -> State CompileState [Opcode]
genFunctionAsm f = do
  fm <- functions <$> get
  let numArgs = length $ getFunArgs f
      paramMap = M.fromList (getFunParamVars f)
  modify $ \c -> c{numVars = numArgs, minVars = numArgs}
  modify $ \c -> c{variables = (variables c) `M.union` paramMap}
  fd <- addOp $ OpFunDef (fm M.! getFunName f) (max 0 (numArgs - 1))
  fds <- genExprAsm (getFunExp f)
  fe <- addOp $ if numArgs == 0 then OpFunEnd else OpRet1
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
  elseBr <- genExprAsm e3
  elseDrops <- addDrops
  _  <- addOp $ OpBr 0   -- placeholder
  l2 <- currPos <$> get
  thenBr <- genExprAsm e2
  thenDrops <- addDrops
  l3 <- currPos <$> get
  let br1 = [OpBrNz l2]
      br2 = [OpBr l3]
  return $ concat [o1, br1, elseDrops, elseBr, br2, thenDrops, thenBr]

genExprAsm (FunApp fn eps) = do
  fm <- functions <$> get
  if null eps
    then do
      vars <- variables <$> get
      case M.lookup fn vars of
        Nothing -> 
          case M.lookup fn fm of
            Nothing -> error $ "Variable \"" ++ fn ++ "\" not defined"
            Just v  -> sequence [addOp $ OpFunCall v]
        Just v  -> genVarAsm v
    else do
      params <- concat <$> mapM genExprAsm (reverse eps)
      fcall <- case M.lookup fn fm of
                 Nothing -> error $ "Function \"" ++ fn ++ "\" not defined"
                 Just i  -> addOp $ OpFunCall i
      return $ params ++ [fcall]

genExprAsm (DataCons consname exps) = do
  consMap <- constructorMap <$> get
  case M.lookup consname consMap of
    Nothing -> do
      vars <- variables <$> get
      case M.lookup consname vars of
        Nothing -> error $ "Internal compiler error during code generation: undeclared data constructor " ++ consname
        Just v  -> genVarAsm v
    Just dtt -> do
      dataszs <- datasizes <$> get
      case M.lookup (dataDeclName dtt) dataszs of
        Nothing       -> error $ "Internal compiler error during code generation: unknown data type " ++ dataDeclName dtt
        Just datasize -> do
          osz <- addOp $ OpInt datasize
          onew <- addOp OpNew
          consasm <-
            if not (hasMultipleConstructors dtt)
              then return []
              else
                case findIndex (\c -> constructorName c == consname) (dataConstructors dtt) of
                  Nothing -> error $ "Internal compiler error during code generation: unknown data constructor " ++ consname
                  Just i  -> do
                    odup <- addOp OpDup
                    cidx <- addOp $ OpInt 0
                    ea   <- addOp $ OpInt i
                    cst  <- addOp OpRStore
                    return [odup, cidx, ea, cst]
          params <- forM (zip [0..] exps) $ \(i, e) -> do
            odup <- addOp OpDup
            oidx <- addOp $ OpInt $ i * 4
            ea <- genExprAsm e
            ost <- addOp OpRStore
            return $ [odup, oidx] ++ ea ++ [ost]
          return $ [osz, onew] ++ consasm ++ concat params

genExprAsm (CmpLt e1 e2) = genArithAsm OpLT e1 e2
genExprAsm (CmpLe e1 e2) = genArithAsm OpLE e1 e2
genExprAsm (CmpEq e1 e2) = genArithAsm OpEQ e1 e2
genExprAsm (Negate (Int i)) = addVar >> sequence [addOp $ OpInt (-i)]
genExprAsm (Negate n)       = genArithAsm OpMul (Int (-1)) n

genVarAsm :: AsmVariable -> State CompileState [Opcode]
genVarAsm (StackVariable v) = do
  addOp (OpLoad (-v)) >>= return . (:[])
genVarAsm (ExpVariable e)   = genExprAsm e
genVarAsm (RefVariable rs)  =
  concat <$> (forM rs $ \(stp, addr) -> do
    st <- addOp $ OpLoad (-stp)
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
  modify $ \c -> c{currPos = currPos c + opLength op}
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

createFunctionMap :: [Function] -> M.Map String Int
createFunctionMap = M.adjust (const 1) "main" . fst . foldl' go (M.empty, 2)
  where go (mp, nr) f = (M.insertWithKey (\k _ _ -> error $ "Function \"" ++ k ++ "\" already defined.")
                               (getFunName f) nr mp, succ nr)

