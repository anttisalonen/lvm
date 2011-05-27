module StauTypes(preludeVariables,
  preludeTypes,
  Value(..),
  ValueMap,
  ExpType(..),
  buildMapsFrom,
  pair, assocsBy)
where

{- The definitions needed by Asm and TypeCheck. -}

import Data.List
import qualified Data.Map as M

import Stau

preludeVariables :: ValueMap
preludeVariables = M.fromList [("True", ExpValue (Int 1) TypeBool), ("False", ExpValue (Int 0) TypeBool)]

preludeTypes :: [DataDecl]
preludeTypes = [DataDecl "Bool" [Constructor "False" [], Constructor "True" []]]

data Value = StackValue Int ExpType
           | ExpValue Exp ExpType

type ValueMap = M.Map String Value

data ExpType = TypeInt
             | TypeBool
             | CustomType String
             | TypeFun [ExpType]
  deriving (Eq)

instance Show ExpType where
  show TypeInt  = "Int"
  show TypeBool = "Bool"
  show (CustomType n) = n
  show (TypeFun es)   = intercalate " -> " (map show es)

assocsBy :: (a -> b) -> [a] -> [(a, b)]
assocsBy f xs = zip xs (map f xs)

pair :: [(a, [b])] -> [(a, b)]
pair [] = []
pair ((x, y:ys):rest) = (x, y) : pair ((x, ys):rest)
pair ((_, []):rest) = pair rest

buildMapFrom :: (Ord b) => (a -> b) -> a -> M.Map b a
buildMapFrom f x = M.fromList [(f x, x)]

buildMapsFrom :: (Ord b) => (a -> b) -> [a] -> M.Map b a
buildMapsFrom f = M.unions . map (buildMapFrom f)


