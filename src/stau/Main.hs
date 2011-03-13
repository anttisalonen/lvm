module Main
where

import Data.List
import qualified Data.Map as M
import Control.Applicative
import Control.Monad
import Control.Monad.State
import Control.Monad.Error()
import System.Exit
import System.Environment
import System.Console.GetOpt

import ParseStau
import Stau

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

main :: IO ()
main = do
    args <- getArgs
    let (actions, nonopts, errs) = getOpt Permute options args
        opts = foldl' (flip ($)) defaultOptions actions
    when (not (null errs) || length nonopts /= 1 || null (outputfile opts)) $ do
        mapM_ putStrLn errs
        pr <- getProgName
        putStrLn $ usageInfo ("Usage: " ++ pr ++ " <options> <input file>") options
        exitWith (ExitFailure 1)
    let ifile = head nonopts
        ofile = outputfile opts
    input <- readFile ifile
    when (showAST opts) $ do
      case getAST input of
        Right ast -> putStrLn ast
        Left  err -> putStrLn err >> exitWith (ExitFailure 1)
    case compile input of
      Right res -> writeFile ofile res
      Left err  -> putStrLn err >> exitWith (ExitFailure 1)

getAST :: String -> Either String String
getAST s = stauLexer s >>= parseStau >>= return . concatMap show
compile :: String -> Either String String
compile s = stauLexer s >>= parseStau >>= return . concatMap show . generateAssembly

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
            | OpFunCall Int
            | OpBr Int
            | OpBrNz Int

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
  show (OpFunCall i) = addLF $ "FUNCALL " ++ show i
  show (OpBr i)      = addLF $ "BR " ++ show i
  show (OpBrNz i)    = addLF $ "BRNZ " ++ show i

generateAssembly :: [Function] -> [Opcode]
generateAssembly fns = evalState (generateAssembly' fns) $ CompileState 0 0 0

generateAssembly' :: [Function] -> State CompileState [Opcode]
generateAssembly' fns = 
  let fm = createFunctionMap fns
  in concat `liftM` forM fns (genFunctionAsm fm)

type FunctionMap = M.Map String Int

data CompileState = CompileState {
    currPos :: Int
  , numVars :: Int
  , minVars :: Int
  }

genFunctionAsm :: FunctionMap -> Function -> State CompileState [Opcode]
genFunctionAsm fm f = do
  let numArgs = length $ getFunArgs f
  modify $ \c -> c{numVars = numArgs, minVars = numArgs}
  fd <- addOp $ OpFunDef (fm M.! getFunName f)
  fds <- genExprAsm fm (getFunExp f)
  fe <- addOp $ OpFunEnd
  drops <- addDrops
  return $ fd : fds ++ drops ++ [fe]

manyOps :: (Monad m) => [m [a]] -> m [a]
manyOps []     = return []
manyOps (n:ns) = do
  p <- n
  ps <- manyOps ns
  return $ concat [p,ps]

genArithAsm :: FunctionMap -> Opcode -> Exp -> Exp -> State CompileState [Opcode]
genArithAsm fm op e1 e2 = do
  a1 <- genExprAsm fm e1
  a2 <- genExprAsm fm e2
  rmVar
  opc <- addOp op
  return $ a1 ++ a2 ++ [opc]

genExprAsm :: FunctionMap -> Exp -> State CompileState [Opcode]
genExprAsm fm (Plus e1 e2)  = genArithAsm fm OpAdd e1 e2
genExprAsm fm (Minus e1 e2) = genArithAsm fm OpSub e1 e2
genExprAsm fm (Times e1 e2) = genArithAsm fm OpMul e1 e2
genExprAsm fm (Div e1 e2)   = genArithAsm fm OpDiv e1 e2
genExprAsm _  (Int i)       = addVar >> sequence [addOp $ OpInt i]
genExprAsm fm (Brack e)     = genExprAsm fm e

genExprAsm fm (IfThenElse e1 e2 e3) = do
  o1 <- genExprAsm fm e1
  rmVar
  _  <- addOp $ OpBrNz 0 -- placeholder
  elseBr <- genExprAsm fm e3
  elseDrops <- addDrops
  _  <- addOp $ OpBr 0   -- placeholder
  l2 <- currPos <$> get
  thenBr <- genExprAsm fm e2
  thenDrops <- addDrops
  l3 <- currPos <$> get
  let br1 = [OpBrNz l2]
      br2 = [OpBr l3]
  return $ concat [o1, br1, elseDrops, elseBr, br2, thenDrops, thenBr]

genExprAsm fm (FunApp fn ep) = do
  param <- genExprAsm fm ep
  fcall <- case M.lookup fn fm of
             Nothing -> error $ "Function \"" ++ fn ++ "\" not defined"
             Just i  -> do
               fc <- addOp $ OpFunCall i
               nv <- numVars <$> get
               if nv < 2
                 then return [fc]
                 else do
                   swp <- addOp $ OpSwap
                   return [fc, swp]
  return $ param ++ fcall

genExprAsm _  (Var _)      = do
  numDrops <- numVars <$> get
  if numDrops <= 1
    then addVar >> sequence [addOp OpDup]
    else return []

genExprAsm fm (CmpLt e1 e2) = genArithAsm fm OpLT e1 e2
genExprAsm fm (CmpLe e1 e2) = genArithAsm fm OpLE e1 e2
genExprAsm fm (CmpEq e1 e2) = genArithAsm fm OpEQ e1 e2
genExprAsm fm (Negate n)    = genArithAsm fm OpMul (Int (-1)) n

addVar, rmVar :: State CompileState ()
addVar = modify $ \c -> c{numVars = succ (numVars c)}
rmVar  = modify $ \c -> c{numVars = pred (numVars c)}

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

asmLength :: [Opcode] -> Int
asmLength = foldl' (+) 0 . map opLength

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
opLength (OpFunCall _) = 5
opLength (OpBr _)      = 5
opLength (OpBrNz _)    = 5

createFunctionMap :: [Function] -> M.Map String Int
createFunctionMap = M.adjust (const 1) "main" . fst . foldl' go (M.empty, 2)
  where go (mp, nr) f = (M.insertWithKey (\k _ _ -> error $ "Function \"" ++ k ++ "\" already defined.")
                               (getFunName f) nr mp, succ nr)

