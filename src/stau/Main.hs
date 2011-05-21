module Main(main)
where

import Data.List
import Control.Monad
import Control.Monad.Error()
import System.IO
import System.Exit
import System.Environment
import System.Console.GetOpt

import ParseStau
import Stau
import TypeCheck
import Asm

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
    input <- if ifile == "-" then getContents else readFile ifile
    let eAst = getAST input
    case eAst of
      Left  err -> do
        let lexed = stauLexer input
        print lexed
        print (lexed >>= return . correctLayout)
        boom err
      Right ast -> do
        when (showAST opts) $ do
          putStrLn ((intercalate "\n" . map show) $ moduleDataDecls ast)
          putStrLn ((intercalate "\n" . map show) $ moduleSignatures ast)
          putStrLn ((intercalate "\n" . map show) $ moduleFunctions ast)
        case typeCheck ast of
          Left err -> boom $ "Type error: " ++ err
          Right _  -> return ()

        case compile ast of
          Right res -> if ofile == "-"
                         then putStr res
                         else writeFile ofile res
          Left err  -> boom err

getAST :: String -> Either String Module
getAST s = stauLexer s >>= parseStau . correctLayout

compile :: Module -> Either String String
compile = return . concatMap show . generateAssembly


