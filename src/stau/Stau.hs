module Stau
where

import Data.Monoid

{- Here are the definitions that are needed by the parser. -}

data Module = Module {
    moduleFunctions  :: [Function]
  , moduleSignatures :: [FunSig]
  , moduleDataDecls  :: [DataDecl]
  }
  deriving (Show)

instance Monoid Module where
  m1 `mappend` m2 = Module (moduleFunctions m1 `mappend` moduleFunctions m2)
                           (moduleSignatures m1 `mappend` moduleSignatures m2)
                           (moduleDataDecls m1 `mappend` moduleDataDecls m2)
  mempty = Module mempty mempty mempty

data Function = Function { 
    getFunName :: String
  , getFunArgs :: [ParamDecl]
  , getFunExp  :: Exp 
  }
  deriving (Show)

data FunSig = FunSig {
    getFunSigName  :: String
  , getFunSigTypes :: [String]
  }
  deriving (Show)

data DataDecl = DataDecl {
    dataDeclName     :: String
  , dataConstructors :: [Constructor]
  }
  deriving (Show)

data Constructor = Constructor {
    constructorName :: String
  , dataFieldTypes  :: [String]
  }
  deriving (Show)

data ParamDecl = VariableParam String
               | ConstructorParam String [ParamDecl]
               | WildcardParam
  deriving (Show)

data Exp
      = Plus Exp Exp 
      | Minus Exp Exp 
      | Times Exp Exp 
      | Div Exp Exp 
      | CmpEq Exp Exp 
      | CmpLt Exp Exp 
      | CmpLe Exp Exp 
      | Int Int 
      | FunApp String [Exp]
      | DataCons String [Exp]
      | Brack Exp
      | Negate Exp
      | IfThenElse Exp Exp Exp
      | CaseOf Exp [CasePattern]
  deriving (Show)

data CasePattern = Case {
    caseParams :: ParamDecl
  , caseExp    :: Exp
  }
  deriving (Show)

type TokenInfo = (Int, Int, Token)

data Token
      = TokenInt Int
      | TokenVar String
      | TokenTypeName String
      | TokenEq
      | TokenPlus
      | TokenMinus
      | TokenTimes
      | TokenDiv
      | TokenCmpEq
      | TokenCmpLt
      | TokenCmpLe
      | TokenIf
      | TokenThen
      | TokenElse
      | TokenOB
      | TokenCB
      | TokenOCurly
      | TokenCCurly
      | TokenEndline
      | TokenFunctionType
      | TokenTypeOf
      | TokenPipe
      | TokenData
      | TokenCons String
      | TokenWildcard
      | TokenCase
      | TokenOf
      | TokenSemicolon
  deriving (Eq, Show)


