module Main(main)
where

import Data.List
import qualified Data.Map as M
import Control.Applicative
import Control.Monad
import Control.Monad.State
import Control.Monad.Error()
import System.IO
import System.Exit
import System.Environment
import System.Console.GetOpt

import ParseStau
import Stau
import StauTypes
import TypeCheck

data Options = Options
  {
    outputfile        :: FilePath
  , showAST           :: Bool
  }
  deriving (Show)

defaultOptions :: Options
defaultOptions = Options "" False

options :: [OptDescr (Options -> Options)]
options = [
    Option ['o'] []                (ReqArg (\l o -> o{outputfile = l}) "output file")   "output file"
  , Option ['a'] []                (NoArg  (\o -> o{showAST = True}))                   "show AST"
  ]

putErrLn :: String -> IO ()
putErrLn = hPutStrLn stderr

boom :: String -> IO ()
boom errMsg = putErrLn errMsg >> exitWith (ExitFailure 1)

main :: IO ()
main = do
    args <- getArgs
    let (actions, nonopts, errs) = getOpt Permute options args
        opts = foldl' (flip ($)) defaultOptions actions
    when (not (null errs) || length nonopts /= 1 || null (outputfile opts)) $ do
        mapM_ putErrLn errs
        pr <- getProgName
        putErrLn $ usageInfo ("Usage: " ++ pr ++ " <options> <input file>") options
        exitWith (ExitFailure 1)
    let ifile = head nonopts
        ofile = outputfile opts
    input <- readFile ifile
    let eAst = getAST input
    case eAst of
      Left  err -> boom err
      Right ast -> do
        when (showAST opts) $ putStrLn ((intercalate "\n" . map show) $ fst ast)
        case typeCheck ast of
          Left err -> boom $ "Type error: " ++ err
          Right _  -> return ()

        case compile (fst ast) of
          Right res -> writeFile ofile res
          Left err  -> boom err

getAST :: String -> Either String ([Function], [FunSig])
getAST s = stauLexer s >>= parseStau

compile :: [Function] -> Either String String
compile = return . concatMap show . generateAssembly

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
            | OpFunDef Int
            | OpFunEnd
            | OpRet0
            | OpRet1
            | OpFunCall Int
            | OpBr Int
            | OpBrNz Int
            | OpLoad Int

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
  show (OpFunDef i)  = addLF $ "FUNDEF " ++ show i
  show OpFunEnd      = addLF $ "FUNEND"
  show OpRet0        = addLF $ "RET0"
  show OpRet1        = addLF $ "RET1"
  show (OpFunCall i) = addLF $ "FUNCALL " ++ show i
  show (OpBr i)      = addLF $ "BR " ++ show i
  show (OpBrNz i)    = addLF $ "BRNZ " ++ show i
  show (OpLoad i)    = addLF $ "LOAD " ++ show i

generateAssembly :: [Function] -> [Opcode]
generateAssembly fns = evalState (generateAssembly' fns) $ CompileState 0 0 0 fm preludeVariables
  where fm = createFunctionMap fns

generateAssembly' :: [Function] -> State CompileState [Opcode]
generateAssembly' fns = 
  concat `liftM` forM fns genFunctionAsm

data CompileState = CompileState {
    currPos :: Int
  , numVars :: Int
  , minVars :: Int
  , functions :: FunctionMap
  , variables :: ValueMap
  }

genFunctionAsm :: Function -> State CompileState [Opcode]
genFunctionAsm f = do
  fm <- functions <$> get
  let numArgs = length $ getFunArgs f
      paramMap = M.fromList $ zip (getFunArgs f) (map (flip StackValue TypeInt) [1..])
  modify $ \c -> c{numVars = numArgs, minVars = numArgs}
  modify $ \c -> c{variables = (variables c) `M.union` paramMap}
  fd <- addOp $ OpFunDef (fm M.! getFunName f)
  fds <- genExprAsm (getFunExp f)
  fe <- addOp $ if getFunName f == "main" then OpFunEnd else OpRet1
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
  if null eps
    then do
      vars <- variables <$> get
      case M.lookup fn vars of
        Nothing -> error $ "Variable \"" ++ fn ++ "\" not defined"
        Just (StackValue v _) -> do
          addOp (OpLoad (-v)) >>= return . (:[])
        Just (ExpValue e _)   -> genExprAsm e
    else do
      params <- concat <$> mapM genExprAsm (reverse eps)
      fm <- functions <$> get
      fcall <- case M.lookup fn fm of
                 Nothing -> error $ "Function \"" ++ fn ++ "\" not defined"
                 Just i  -> do
                   addOp $ OpFunCall i
      return $ params ++ [fcall]

genExprAsm (CmpLt e1 e2) = genArithAsm OpLT e1 e2
genExprAsm (CmpLe e1 e2) = genArithAsm OpLE e1 e2
genExprAsm (CmpEq e1 e2) = genArithAsm OpEQ e1 e2
genExprAsm (Negate (Int i)) = addVar >> sequence [addOp $ OpInt (-i)]
genExprAsm (Negate n)       = genArithAsm OpMul (Int (-1)) n

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
opLength (OpFunDef _)  = 5
opLength OpFunEnd      = 1
opLength OpRet0        = 1
opLength OpRet1        = 1
opLength (OpFunCall _) = 5
opLength (OpBr _)      = 5
opLength (OpBrNz _)    = 5
opLength (OpLoad _)    = 5

createFunctionMap :: [Function] -> M.Map String Int
createFunctionMap = M.adjust (const 1) "main" . fst . foldl' go (M.empty, 2)
  where go (mp, nr) f = (M.insertWithKey (\k _ _ -> error $ "Function \"" ++ k ++ "\" already defined.")
                               (getFunName f) nr mp, succ nr)

