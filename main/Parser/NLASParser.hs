module Parser.NLASParser where
import           Data.Char                     (isLower)
import           Expression
import           PermutationInstances
import           Text.ParserCombinators.Parsec

upFile :: GenParser Char st [[Expression (SwappingList String) String]]
upFile = do result <- many line
            eof
            return result

line = do result <- eq
          eol
          return result

eq = do first <- expression
        rest <- remainigExpressions
        return (first:rest)

remainigExpressions = (char '=' >> eq) <|> return []

eol = char '\n' <|> char ';'

expression = do spaces
                result <- expression0
                spaces
                return result

expression0 = lambdaEx <|>
              funEx <|>
              try suspension <|>
              braces expression

funEx = do fn <- fnName
           args <- many expression
           return (Fn fn args)

lambdaEx = do lamSymb
              spaces
              a <- atomTuple
              spaces
              dotSymb
              e <- expression
              return (Lam a e)

suspension = do perm <- swappingList
                spaces
                n <- name
                return (if isLower . head $ n then AtomSuspension perm (AtVar (Name n))
                                              else ExpressionSuspension perm (ExVar (Name n)))

-- Parses an atom tuple
atomTuple = do perm <- swappingList
               spaces
               a <- atom
               return (perm, a)

swappingList = many swapping0

swapping0 = do res <- swapping
               spaces
               return res

swapping = braces (do a <- atom
                      skipMany1 space
                      b <- atom
                      return (a, b))

atom = do first <- lower
          rest <- many alphaNum
          return (AtVar (Name (first:rest)))

name = do first <- letter
          rest <- many alphaNum
          return (first:rest)

fnName = do first <- char '$'
            rest <- many (noneOf "()$\\ ")
            return (first:rest)

lamSymb = char '\\'
dotSymb = char '-' >> char '>'

braces  = between (char '(') (char ')')
