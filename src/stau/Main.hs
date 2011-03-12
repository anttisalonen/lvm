module Main
where

import Data.List
import qualified Data.Map as M
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
  }
  deriving (Show)

defaultOptions :: Options
defaultOptions = Options ""

options :: [OptDescr (Options -> Options)]
options = [
    Option ['o'] []                (ReqArg (\l o -> o{outputfile = l}) "output file")   "output file"
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
    case compile input of
      Right res -> writeFile ofile res
      Left err  -> putStrLn err

compile :: String -> Either String String
compile s = stauLexer s >>= parseStau >>= return . concatMap show . generateAssembly

data Opcode = OpInt Int
            | OpAdd
            | OpSub
            | OpMul
            | OpDiv
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
  show (OpFunDef i)  = addLF $ "FUNDEF " ++ show i
  show OpFunEnd      = addLF $ "FUNEND"
  show (OpFunCall i) = addLF $ "FUNCALL " ++ show i
  show (OpBr i)      = addLF $ "BR " ++ show i
  show (OpBrNz i)    = addLF $ "BRNZ " ++ show i

generateAssembly :: [Function] -> [Opcode]
generateAssembly fns = evalState (generateAssembly' fns) 0

generateAssembly' :: [Function] -> State Int [Opcode]
generateAssembly' fns = 
  let fm = createFunctionMap fns
  in concat `liftM` forM fns (genFunctionAsm fm)

genFunctionAsm :: M.Map String Int -> Function -> State Int [Opcode]
genFunctionAsm fm f = do
  fd <- addOp $ OpFunDef (fm M.! getFunName f)
  fds <- genExprAsm (getFunExp f)
  fe <- addOp $ OpFunEnd
  return $ fd : fds ++ [fe]

manyOps :: (Monad m) => [m [a]] -> m [a]
manyOps []     = return []
manyOps (n:ns) = do
  p <- n
  ps <- manyOps ns
  return $ concat [p,ps]

genExprAsm :: Exp -> State Int [Opcode]
genExprAsm (Plus e1 e2)  = manyOps [genExprAsm e1, genExprAsm e2, sequence [addOp OpAdd]]
genExprAsm (Minus e1 e2) = manyOps [genExprAsm e1, genExprAsm e2, sequence [addOp OpSub]]
genExprAsm (Times e1 e2) = manyOps [genExprAsm e1, genExprAsm e2, sequence [addOp OpMul]]
genExprAsm (Div e1 e2)   = manyOps [genExprAsm e1, genExprAsm e2, sequence [addOp OpDiv]]
genExprAsm (Int i)       = sequence [addOp $ OpInt i]
genExprAsm (Brack e)     = genExprAsm e
genExprAsm (IfThenElse e1 e2 e3) = do
  o1 <- genExprAsm e1
  _  <- addOp $ OpBrNz 0 -- placeholder
  thenBr <- genExprAsm e3
  _  <- addOp $ OpBr 0   -- placeholder
  l2 <- get
  elseBr <- genExprAsm e2
  l3 <- get
  let br1 = [OpBrNz l2]
      br2 = [OpBr l3]
  return $ concat [o1, br1, elseBr, br2, thenBr]
genExprAsm e            = error $ "Expression '" ++ show e ++ "' not supported yet"

addOp :: Opcode -> State Int Opcode
addOp op = do
  modify (+ (opLength op))
  return op

asmLength :: [Opcode] -> Int
asmLength = foldl' (+) 0 . map opLength

opLength :: Opcode -> Int
opLength (OpInt _)     = 5
opLength OpAdd         = 1
opLength OpSub         = 1
opLength OpMul         = 1
opLength OpDiv         = 1
opLength (OpFunDef _)  = 5
opLength OpFunEnd      = 1
opLength (OpFunCall _) = 5
opLength (OpBr _)      = 5
opLength (OpBrNz _)    = 5

createFunctionMap :: [Function] -> M.Map String Int
createFunctionMap = M.adjust (const 1) "main" . fst . foldl' go (M.empty, 2)
  where go (mp, nr) f = (M.insertWithKey (\k _ _ -> error $ "Function \"" ++ k ++ "\" already defined.")
                               (getFunName f) nr mp, succ nr)

