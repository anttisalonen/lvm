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
      case            { TokenCase }
      of              { TokenOf }
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
      '{'             { TokenOCurly }
      '}'             { TokenCCurly }
      '->'            { TokenFunctionType }
      '::'            { TokenTypeOf }
      '|'             { TokenPipe }
      ';'             { TokenSemicolon }
      '_'             { TokenWildcard }
%%

ModuleDecls :: { Module }
ModuleDecls : ModuleDecls ';' ModuleDecl { $3 `mappend` $1 }
            | ModuleDecls ';'            { $1 }
            | ModuleDecl                 { $1 }
            | {- empty -}                { Module [] [] [] }

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

-- TODO: the type should be Tree String for function types
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
      | '_'                     { WildcardParam }

CaseParam :: { ParamDecl }
CaseParam : '(' var ')'             { VariableParam $2 }
          | '(' typename Params ')' { ConstructorParam $2 $3 }
          | var                     { VariableParam $1 }
          | typename Params         { ConstructorParam $1 $2 }
          | '_'                     { WildcardParam }

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
     | case Exp of '{' CasePatterns '}' { CaseOf $2 $5 }

CasePatterns :: { [CasePattern] }
CasePatterns : CasePattern ';' CasePatterns { $1 : $3 }
             | ';' CasePatterns             { $2 }
             | CasePattern ';'              { [$1] }
             | CasePattern                  { [$1] }

CasePattern :: { CasePattern }
CasePattern : CaseParam '->' Exp { Case $1 $3 }

Atom :: { Exp }
Atom : int %prec VAR            { Int $1 }
     | '(' Exp ')'              { Brack $2 }
     | typename                 { DataCons $1 [] }
     | var                      { FunApp $1 [] }

{

concatEithers :: ([a], [b]) -> Either a b -> ([a], [b])
concatEithers (as, bs) (Left a)  = (a:as, bs)
concatEithers (as, bs) (Right b) = (as, b:bs)

parseError :: [Token] -> Either String a
parseError ts = Left $ "Parse error: " ++ show ts

isEndline :: Char -> Bool
isEndline '\n' = True
isEndline _    = False

comb :: Int -> String -> TokenInfo -> Either String [TokenInfo]
comb num a b@(line, col, tok) = case stauLexer' line (col + num) a of
             Left err -> Left err
             Right n  -> Right (b : n)

correctLayout :: [TokenInfo] -> [Token]
correctLayout = correctLayout' 1

correctLayout' _ [] = []
correctLayout' n ((line, col, TokenOf):ti2@(_, col2, t2):ts)
  | t2 /= TokenOCurly = TokenOf : TokenOCurly : t2 : correctLayout' col2 ts
correctLayout' n ((_, _, TokenSemicolon):(_, _, TokenSemicolon):ts) = TokenSemicolon : correctLayout' n ts
correctLayout' n (ti@(line, col, t):ts) 
  | col > n   = t : correctLayout' n ts
  | col == n  = TokenSemicolon : t : correctLayout' n ts
  | otherwise = TokenCCurly : correctLayout' 1 ((line, col, TokenSemicolon) : ti : ts)

advToken :: Int -> Int -> Int -> Token -> String -> Either String [TokenInfo]
advToken num line col tok cs = comb num cs (line, col, tok)

stauLexer :: String -> Either String [TokenInfo]
stauLexer = stauLexer' 1 1

stauLexer' :: Int -> Int -> String -> Either String [TokenInfo]
stauLexer' line col _ | line `seq` col `seq` False = undefined
stauLexer' _ _ [] = Right []
stauLexer' line col (c:cs) 
      | isEndline c = stauLexer' (succ line) 1 cs
      | isSpace c   = stauLexer' line (succ col) cs
      | isAsciiLower c || c == '_' = lexVar line col (c:cs)
                                       [("if",   TokenIf),
                                        ("then", TokenThen), 
                                        ("else", TokenElse),
                                        ("case", TokenCase),
                                        ("of",   TokenOf),
                                        ("data", TokenData)]
                                       (\v -> if v == "_" then TokenWildcard else TokenVar v)
      | isAsciiUpper c = lexVar line col (c:cs)
                             [] TokenTypeName
      | isDigit c = lexNum line col (c:cs)
stauLexer' line col ('=':'=':cs) = advToken 2 line col TokenCmpEq cs
stauLexer' line col ('<':'=':cs) = advToken 2 line col TokenCmpLe cs
stauLexer' line col ('-':'>':cs) = advToken 2 line col TokenFunctionType cs
stauLexer' line col (':':':':cs) = advToken 2 line col TokenTypeOf cs
stauLexer' line col ('|':cs) = advToken 1 line col TokenPipe cs
stauLexer' line col ('<':cs) = advToken 1 line col TokenCmpLt cs
stauLexer' line col ('=':cs) = advToken 1 line col TokenEq cs
stauLexer' line col ('+':cs) = advToken 1 line col TokenPlus cs
stauLexer' line col ('-':cs) = advToken 1 line col TokenMinus cs
stauLexer' line col ('*':cs) = advToken 1 line col TokenTimes cs
stauLexer' line col ('/':cs) = advToken 1 line col TokenDiv cs
stauLexer' line col ('(':cs) = advToken 1 line col TokenOB cs
stauLexer' line col (')':cs) = advToken 1 line col TokenCB cs
stauLexer' line col ('{':cs) = advToken 1 line col TokenOCurly cs
stauLexer' line col ('}':cs) = advToken 1 line col TokenCCurly cs
stauLexer' line col (';':cs) = advToken 1 line col TokenSemicolon cs
stauLexer' line col (c:_) = Left $ "stauLexer' says: Syntax error at `" ++ [c] ++ "'"

lexNum line col cs = advToken (length num) line col (TokenInt (read num)) rest
      where (num,rest) = span isDigit cs

lexVar line col cs cons def = advToken (length tok) line col var rest
  where (tok, rest) = span (\c -> isAlphaNum c || c == '_') cs
        var         = case lookup tok cons of
                        Just cd -> cd
                        Nothing -> def tok

}

