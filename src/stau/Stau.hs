module Stau
where

data Function = Function { 
  getFunName :: String,
  getFunArgs :: [String],
  getFunExp :: Exp }
      deriving Show

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
      | Brack Exp
      | Negate Exp
      | IfThenElse Exp Exp Exp
      deriving Show

data Token
      = TokenInt Int
      | TokenVar String
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
      | TokenEndline
 deriving Show


