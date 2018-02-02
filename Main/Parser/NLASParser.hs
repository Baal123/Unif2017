module Parser.NLASParser(fromText) where
import           Data.Char                     (isLower)
import           Expression
import           Constraint
import           PermutationInstances
import           Text.ParserCombinators.Parsec
import           UnificationProblem
import           UnificationContext
import           Compression

type Pi = SwappingList String
data ConOrEq = E [Expression Pi String]
               | C [Constraint Pi String]
         | S String
               deriving (Eq, Show, Ord)

-- General
fromText text = fmap (\up -> simpleUp up ([], [])) (lsFromText text)

simpleUp [] (gamma, nabla) = (reverse gamma, reverse nabla)
simpleUp (E eq:ls) (gamma, nabla) = simpleUp ls (eq:gamma, nabla)
simpleUp (C c:ls) (gamma, nabla) = simpleUp ls (gamma, c ++ nabla)
simpleUp (_:ls) up = simpleUp ls up

lsFromText = parse up ""
-- Parser
up :: GenParser Char st [ConOrEq]
up = do result <- line `sepBy` eol
        eof
        return result

line = comment <|> try conL <|> eqL

eol =  char ';' <|> char '\n' <|> try (char '\r' >> char '\n') <|> char '\r'

comment = do whiteSpaces
             string "--" 
             result <- many anyChar 
             return (S result)                    

-- Constraints
conL = do whiteSpaces
          a <- atom
          whiteSpaces
          char '#'
          es <- expression `sepBy` char ','
          return (C (map (Fresh a) es))

-- Equations
eqL = do result <- eq
         return (E result)

eq = expression `sepBy` char '='

-- Expressions
expression = do whiteSpaces
                result <- expression0
                whiteSpaces
                return result

expression0 = lambdaEx <|>
              funEx <|>
              try suspension <|>
              braces expression

-- All parsers from here on should neithe begin nor end with "whiteSpaces"
funEx = do fn <- fnName
           args <- many (try expression)
           return (Fn fn args)

lambdaEx = do lamSymb
              whiteSpaces
              a <- atomTuple
              whiteSpaces
              dotSymb
              e <- expression
              return (Lam a e)

suspension = do perm <- swappingList
                whiteSpaces
                n <- name
                return (if isLower . head $ n then AtomSuspension perm (AtVar (Name n))
                                              else ExpressionSuspension perm (ExVar (Name n)))

atomTuple = do perm <- swappingList
               whiteSpaces
               a <- atom
               return (perm, a)

swappingList = many swapping0

swapping0 = do res <- swapping
               whiteSpaces
               return res

swapping = braces (do a <- atom
                      skipMany1 whiteSpace
                      b <- atom
                      return (a, b))

atom = do first <- lower
          rest <- many alphaNum
          return (AtVar (Name (first:rest)))

name = do first <- letter
          rest <- many alphaNum
          return (first:rest)

fnName = do first <- char '$'
            rest <- many (noneOf " \n\r;()#")
            return (first:rest)

lamSymb = char '\\'
dotSymb = char '-' >> char '>'

-- Help
braces  = between (char '(') (char ')')

whiteSpace = char ' '
whiteSpaces = skipMany whiteSpace
