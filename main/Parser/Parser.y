{
module Main where
import Data.Char
}

%name calc
%tokentype { Token }
%error { parseError }


%token
      at              { TokenAt $$ }
      '='             { TokenEq }
      '#'             { TokenFresh }
      ','             { TokenComma }
      '('             { TokenOB }
      ')'             { TokenCB }
      '\\'            { TokenLam }
      to              { TokenTo }
      fn              { TokenFn $$ }
      ex              { TokenEx $$ }
      ';'             { TokenSep }
%%
UpEntries : UpEntry                     { [$1] }
          | UpEntries ';' UpEntry       { $3:$1 }
          | UpEntries ';'               {$1}
          | {- empty -}                 { [] }


UpEntry : Exp '=' Exp              { Eq $1 $3 }
         | Atom '#' Exp            { Fresh $1 $3 }

Exp : Swap  Exp                  { Apply $1 $2 }
    | '\\' BExp to Exp           { Lam $2 $4 }
    | Atom                       { ExAtom $1 }
    | ExName                     { ExVar $1 }
    | '(' Exp ')'                { $2 }
    | fn Exps                    { Fn $1 $2 }

Exps : {- empty -}                { [] }
     |  Exp Exps                   { $1:$2 }

BExp  : Swap  BExp               { BaseApply $1 $2 }
      | Atom                     { BExAtom $1 }

Swap : '(' Atom ',' Atom ')'     { Swap $2 $4 }

Atom : at                        { At $1 }
ExName : ex                      { ExN $1 }

{
parseError :: [Token] -> a
parseError tokens = error ("Remaing Tokens: " ++ (show tokens))

data UpEntry
      = Eq Exp Exp
      | Fresh Atom Exp
      deriving Show

data Exp
      = Apply Swap Exp
      | ExAtom Atom
      | ExVar ExpressionName
      | Lam BExp Exp
      | Fn String [Exp]
      deriving Show

data BExp
      = BaseApply Swap BExp
      | BExAtom Atom
      deriving Show

data Swap
      = Swap Atom Atom
      deriving Show

data Atom
      = At String
      deriving Show

data FunctionName
      = FnN String
      deriving Show

data ExpressionName
      = ExN String
      deriving Show

data Token
      = TokenAt String
      | TokenEq
      | TokenFresh
      | TokenComma
      | TokenOB
      | TokenCB
      | TokenLam
      | TokenTo
      | TokenFn String
      | TokenEx String
      | TokenSep
      deriving Show

lexer :: String -> [Token]
lexer [] = []
lexer (c:cs)
      | isSep c = TokenSep : lexer cs
      | isSpace c = lexer cs
      | isAlpha c = lexName (c:cs)
      | c == '$' = lexName (c:cs)

lexer ('=':cs) = TokenEq : lexer cs
lexer ('#':cs) = TokenFresh : lexer cs
lexer (',':cs) = TokenComma : lexer cs
lexer ('(':cs) = TokenOB : lexer cs
lexer (')':cs) = TokenCB : lexer cs
lexer ('\\':cs) = TokenLam : lexer cs
lexer ('-':'>':cs) = TokenTo : lexer cs

isSep ';' = True
isSep '\r' = True
isSep '\n' = True
isSep c = False

lexName (c:cs)
    | isLower c = TokenAt var : lexer rest
    | isUpper c = TokenEx var : lexer rest
    | c == '$' && (isAlpha (head cs)) = TokenFn var : lexer rest
     where
        (var, rest) | c == '$' = span isNameChar cs | otherwise = span isNameChar (c:cs)
        isNameChart '_' = True
        isNameChar c = isAlphaNum c

main = getContents >>= print . calc . lexer
}
