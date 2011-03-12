{
{-# OPTIONS_GHC -w #-}
module ParseStau where

import Control.Monad.Error

import Data.Char

import Stau
}

%name parseStau
%tokentype { Token }
%error { parseError }
%monad { Either String }
%left '+' '-'
%left '*' '/'
%left NEG
%left if
%left then
%left else

%token 
      var             { TokenVar $$ }
      int             { TokenInt $$ }
      if              { TokenIf }
      then            { TokenThen }
      else            { TokenElse }
      '='             { TokenEq }
      '+'             { TokenPlus }
      '-'             { TokenMinus }
      '*'             { TokenTimes }
      '/'             { TokenDiv }
      '('             { TokenOB }
      ')'             { TokenCB }
      '\n'            { TokenEndline }
%%

Functions :: { [Function] }
Functions : Functions '\n' Function { $3 : $1 }
          | Functions '\n'          { $1 }
          | Function                { [$1] }
          | {- empty -}             { [] }

Function :: { Function }
Function : var '=' Exp { Function $1 $3 }

Exp :: { Exp }
Exp  : Exp '+' Exp           { Plus $1 $3 }
     | Exp '-' Exp           { Minus $1 $3 }
     | Exp '*' Exp           { Times $1 $3 }
     | Exp '/' Exp           { Div $1 $3 }
     | int                   { Int $1 }
     | var                   { Var $1 }
     | '(' Exp ')'           { Brack $2 }
     | '-' Exp %prec NEG     { Negate $2 }
     | if Exp then Exp else Exp { IfThenElse $2 $4 $6 }
{

parseError :: [Token] -> Either String a
parseError _ = Left "Parse error"

isEndline :: Char -> Bool
isEndline '\n' = True
isEndline _    = False

comb :: String -> Token -> Either String [Token]
comb a b = case stauLexer a of
             Left err -> Left err
             Right n  -> Right (b : n)

stauLexer :: String -> Either String [Token]
stauLexer [] = Right []
stauLexer (c:cs) 
      | isSpace c && not (isEndline c) = stauLexer cs
      | isAlpha c = lexVar (c:cs)
      | isDigit c = lexNum (c:cs)
stauLexer ('=':cs) = comb cs TokenEq
stauLexer ('+':cs) = comb cs TokenPlus
stauLexer ('-':cs) = comb cs TokenMinus
stauLexer ('*':cs) = comb cs TokenTimes
stauLexer ('/':cs) = comb cs TokenDiv
stauLexer ('(':cs) = comb cs TokenOB
stauLexer (')':cs) = comb cs TokenCB
stauLexer ('\n':cs) = comb cs TokenEndline
stauLexer (c:_) = Left $ "stauLexer says: Syntax error at '" ++ [c] ++ "'"

lexNum cs = comb rest (TokenInt (read num))
      where (num,rest) = span isDigit cs

lexVar cs = comb rest var
  where (tok, rest) = span isAlpha cs
        var         = case tok of
                        "if"   -> TokenIf
                        "then" -> TokenThen
                        "else" -> TokenElse
                        v      -> TokenVar v

}


