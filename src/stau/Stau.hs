module Stau
where

data Function = Function { getFunName :: String, getFunExp :: Exp }
      deriving Show

data Exp
      = Plus Exp Exp 
      | Minus Exp Exp 
      | Times Exp Exp 
      | Div Exp Exp 
      | Int Int 
      | Var String 
      | Brack Exp
      | Negate Exp
      deriving Show

data Token
      = TokenInt Int
      | TokenVar String
      | TokenEq
      | TokenPlus
      | TokenMinus
      | TokenTimes
      | TokenDiv
      | TokenOB
      | TokenCB
      | TokenEndline
 deriving Show


