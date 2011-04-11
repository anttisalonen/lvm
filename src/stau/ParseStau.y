{
{-# OPTIONS_GHC -w #-}
module ParseStau where

import Data.Monoid
import Data.Char
import Data.Either

import Control.Monad.Error

import Stau
}

%name parseStau
%tokentype { Token }
%error { parseError }
%monad { Either String }

%nonassoc if then else
%nonassoc '<=' '<' '=='
%left AP
%left '+' '-'
%left '*' '/'
%left NEG
%left VAR
%left '(' ')'

%token 
      var             { TokenVar $$ }
      typename        { TokenTypeName $$ }
      int             { TokenInt $$ }
      if              { TokenIf }
      then            { TokenThen }
      else            { TokenElse }
      data            { TokenData }
      '=='            { TokenCmpEq }
      '<'             { TokenCmpLt }
      '<='            { TokenCmpLe }
      '='             { TokenEq }
      '+'             { TokenPlus }
      '-'             { TokenMinus }
      '*'             { TokenTimes }
      '/'             { TokenDiv }
      '('             { TokenOB }
      ')'             { TokenCB }
      '\n'            { TokenEndline }
      '->'            { TokenFunctionType }
      '::'            { TokenTypeOf }
      '|'             { TokenPipe }
%%

ModuleDecls :: { Module }
ModuleDecls : ModuleDecls '\n' ModuleDecl { $3 `mappend` $1 }
            | ModuleDecls '\n'            { $1 }
            | ModuleDecl                  { $1 }
            | {- empty -}                 { Module [] [] [] }

ModuleDecl :: { Module }
ModuleDecl : FunDecl  { Module [$1] [] [] }
	   | FunSig   { Module [] [$1] [] }
           | DataDecl { Module [] [] [$1] }

DataDecl :: { DataDecl }
DataDecl : data typename '=' constructors { DataDecl $2 $4 }

constructors :: { [Constructor] }
constructors : constructor '|' constructors { $1 : $3 }
	     | constructor                  { [$1] }

constructor :: { Constructor }
constructor : typename Typenames { Constructor $1 $2 }

Typenames :: { [String] }
Typenames : typename Typenames { $1 : $2 }
	  | {- empty -}        { [] }

FunSig :: { FunSig }
FunSig : var  '::' Types { FunSig $1 $3 }

Types :: { [String] }
Types : typename '->' Types { $1 : $3 }
      | typename            { [$1] }

Params :: { [ParamDecl] }
Params : Param Params { $1 : $2 }
       | {- empty -}  { [] }

Param :: { ParamDecl }
Param : '(' var ')'             { VariableParam $2 }
      | '(' typename Params ')' { ConstructorParam $2 $3 }
      | var                     { VariableParam $1 }
      | typename                { ConstructorParam $1 [] }

FunDecl :: { Function }
FunDecl : var Params '=' Exp { Function $1 $2 $4 }

Atoms :: { [Exp] }
Atoms : {- empty -} { [] }
      | Atom Atoms  { $1 : $2 }

Exp :: { Exp }
Exp  : Exp '+' Exp              { Plus $1 $3 }
     | Exp '-' Exp              { Minus $1 $3 }
     | Exp '*' Exp              { Times $1 $3 }
     | Exp '/' Exp              { Div $1 $3 }
     | Exp '<=' Exp             { CmpLe $1 $3 }
     | Exp '==' Exp             { CmpEq $1 $3 }
     | Exp '<' Exp              { CmpLt $1 $3 }
     | int %prec VAR            { Int $1 }
     | '(' Exp ')'              { Brack $2 }
     | var Atoms %prec AP       { FunApp $1 $2 }
     | typename Atoms %prec AP  { DataCons $1 $2 }
     | '-' Exp %prec NEG        { Negate $2 }
     | if Exp then Exp else Exp { IfThenElse $2 $4 $6 }

Atom :: { Exp }
Atom : int %prec VAR { Int $1 }
     | '(' Exp ')'   { Brack $2 }

{

concatEithers :: ([a], [b]) -> Either a b -> ([a], [b])
concatEithers (as, bs) (Left a)  = (a:as, bs)
concatEithers (as, bs) (Right b) = (as, b:bs)

parseError :: [Token] -> Either String a
parseError ts = Left $ "Parse error: " ++ show ts

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
      | isAsciiLower c = lexVar (c:cs)
                             [("if", TokenIf),
                              ("then", TokenThen), 
                              ("else", TokenElse),
                              ("data", TokenData)] TokenVar
      | isAsciiUpper c = lexVar (c:cs)
                             [] TokenTypeName
      | isDigit c = lexNum (c:cs)
stauLexer ('=':'=':cs) = comb cs TokenCmpEq
stauLexer ('<':'=':cs) = comb cs TokenCmpLe
stauLexer ('-':'>':cs) = comb cs TokenFunctionType
stauLexer (':':':':cs) = comb cs TokenTypeOf
stauLexer ('|':cs) = comb cs TokenPipe
stauLexer ('<':cs) = comb cs TokenCmpLt
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

lexVar cs cons def = comb rest var
  where (tok, rest) = span isAlphaNum cs
        var         = case lookup tok cons of
                        Just cd -> cd
                        Nothing -> def tok

}


