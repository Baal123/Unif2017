module Main where
import           AVMGU
import           Compression
import           Constraint
import           Data.Char
import           Data.Either
import           Display
import           Equation                      (fromList)
import           Flattening
import           Parser.NLASParser             (fromText)
import           PermutationInstances          (SwappingList)
import           Text.ParserCombinators.Parsec (ParseError)
import           UnificationContext
import           UnificationProblem
import           Utilities.CustomUtilities

main :: IO()
main = do putStrLn "Enter Unification Problem - Press '.' to confirm"
          inp <- readUp
          let parsed = parseUP inp
              in if isLeft parsed then putStrLn "Invalid Input"
                 else let up = pureUP parsed
                          in do putStrLn "Unification Problem read as:"
                                printUP up
                                printEmpty 2
                                putStrLn "--------------"
                                printEmpty 1
                                printResult (uncurry avmgu up)
                                end

readUp = readUp0 []
readUp0 ls = do l <- getLine
                if l /= "" && last l == '.' then return (unlines (reverse ls))
                else readUp0 (l:ls)

end = do printEmpty 1
         putStrLn "Press r to repeat"
         a <- getChar
         if toUpper a == 'R' then main
         else putStrLn ""

printUP ((gamma, nabla), c) = do printEqs gamma
                                 printEmpty 2
                                 printNabla nabla
                                 printEmpty 2
                                 printContext c

printResult (Left failure) = putStrLn ("Failue in AVMGU: " ++ show failure)
printResult (Right ((nabla, theta), c)) = do putStrLn "AVMGU result is:"
                                             printTheta theta
                                             printEmpty 2
                                             printNabla nabla
                                             printEmpty 2
                                             printContext c

printTheta theta = do putStrLn "Theta:"
                      putStr (display theta)

printEqs gamma = do putStrLn "Gamma is:"
                    putStr (display . toList $ gamma)

printNabla nabla = do putStrLn "Nabla is:"
                      putStr (display nabla)

printContext con = do putStrLn "Context is:"
                      putStr (display con)

printEmpty 1 = putStrLn ""
printEmpty n = do printEmpty (n-1)
                  putStrLn ""

pureUP :: Either ParseError (LUnificationProblem String, LUnifContext (SwappingList String))
        -> (LUnificationProblem String, LUnifContext (SwappingList String))
pureUP = fromRight

parseUP :: String -> Either ParseError (LUnificationProblem String, LUnifContext (SwappingList String))
parseUP text = fmap (uncurry up) (fromText text)

up eqs nabla = let (eqs0, c0) = compressEquations eqs freshLContext
                   (nablaL, c1) = compressConstraints nabla c0
                   (eqs1, c2) = flattenEqsC eqs0 c1
                   (gamma, c3) = gammaFromList (filter (not . trivial) (concatMap fromList eqs1)) c2 -- Bug in fromList
                   in ((gamma, nablaL), c3)
                where
                  trivial (e, [], t) = True
                  trivial _          = False
