module StauTypes
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
             | TypeFun [ExpType]
  deriving (Eq)

instance Show ExpType where
  show TypeInt  = "Int"
  show TypeBool = "Bool"
  show (TypeFun es) = intercalate " -> " (map show es)


