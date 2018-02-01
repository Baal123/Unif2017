module Parser.NLASParser where
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
               deriving (Eq, Show, Ord)

-- General
simpleUpFromText text = fmap (\up -> simpleUp up ([], [])) (fromText text)

simpleUp [] up = up
simpleUp (E eq:ls) (gamma, nabla) = simpleUp ls (eq:gamma, nabla)
simpleUp (C c:ls) (gamma, nabla) = simpleUp ls (gamma, c:nabla)

fromText = parse up ""
-- Parser
up :: GenParser Char st [ConOrEq]
up = do result <- line `sepBy` eol
        eof
        return result

line = try conL <|> eqL

eol = char '\n' <|> char ';'

-- Constraints
conL = do spaces
          a <- atom
          spaces
          char '#'
          es <- expression `sepBy` char ','
          return (C (map (Fresh a) es))

-- Equations
eqL = do result <- eq
         return (E result)

eq = expression `sepBy` char '='

-- Expressions
expression = do spaces
                result <- expression0
                spaces
                return result

expression0 = lambdaEx <|>
              funEx <|>
              try suspension <|>
              braces expression

-- All parsers from here on should neithe begin nor end with "spaces"
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

-- Help
braces  = between (char '(') (char ')')
