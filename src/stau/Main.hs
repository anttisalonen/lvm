module Main
where

import Data.List
import qualified Data.Map as M
import Control.Monad
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
generateAssembly fns = 
  let fm = createFunctionMap fns
      generateAssembly' f = OpFunDef (fm M.! getFunName f) : genExprAsm (getFunExp f)
      genExprAsm (Plus e1 e2)  = genExprAsm e1 ++ genExprAsm e2 ++ [OpAdd]
      genExprAsm (Minus e1 e2) = genExprAsm e1 ++ genExprAsm e2 ++ [OpSub]
      genExprAsm (Times e1 e2) = genExprAsm e1 ++ genExprAsm e2 ++ [OpMul]
      genExprAsm (Div e1 e2)   = genExprAsm e1 ++ genExprAsm e2 ++ [OpDiv]
      genExprAsm (Int i)      = [OpInt i]
      genExprAsm (Brack e)    = genExprAsm e
      genExprAsm e            = error $ "Expression '" ++ show e ++ "' not supported yet"
  in concatMap generateAssembly' fns

createFunctionMap :: [Function] -> M.Map String Int
createFunctionMap = M.adjust (const 1) "main" . fst . foldl' go (M.empty, 2)
  where go (mp, nr) f = (M.insertWithKey (\k _ _ -> error $ "Function \"" ++ k ++ "\" already defined.")
                               (getFunName f) nr mp, succ nr)

