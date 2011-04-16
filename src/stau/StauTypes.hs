module StauTypes(preludeVariables,
  Value(..),
  ValueMap,
  ExpType(..),
  DataTypeMap,
  buildDataTypeMap)
where

{- The definitions needed by Asm and TypeCheck. -}

import Data.List
import qualified Data.Map as M

import Stau

preludeVariables :: ValueMap
preludeVariables = M.fromList [("True", ExpValue (Int 1) TypeBool), ("False", ExpValue (Int 0) TypeBool)]

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

type DataTypeMap = M.Map String DataDecl

buildDataTypeMap :: Module -> DataTypeMap
buildDataTypeMap ast = buildMultiMaps (map constructorName . dataConstructors) (moduleDataDecls ast)

buildMultiMaps :: (Ord b) => (a -> [b]) -> [a] -> M.Map b a
buildMultiMaps fetch src = M.unions (map (buildMultiMap fetch) src)

buildMultiMap :: (Ord b) => (a -> [b]) -> a -> M.Map b a
buildMultiMap fetch src = M.fromList (zip (fetch src) (repeat src))


