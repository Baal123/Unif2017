module Main where
import Parser.NLASParser(fromText)
import Text.ParserCombinators.Parsec(ParseError)
import AVMGU
import UnificationProblem
import Flattening
import Compression
import UnificationContext
import Equation(fromList)
import PermutationInstances(SwappingList)
import Data.Either
import Utilities.CustomUtilities
import Constraint

main :: IO()
main = do putStrLn "Enter Unification Problem - Press '.' to confirm"
          inp <- readUp
          let parsed = parseUP inp
              in if isLeft parsed then putStrLn "Invalid Input"
                 else let up = pureUP parsed
                          in do putStrLn "Unification Problem read as:"
                                printUP up
                                printResult (uncurry avmgu up)
                                end

readUp = readUp0 []
readUp0 ls = do l <- getLine
                if last l == '.' then return (unlines (reverse ls))
                else readUp0 (l:ls)


end = do putStrLn "Press any key to end"
         getChar
         putStrLn ""

printUP ((gamma, nabla), c) = do printEqs gamma
                                 putStrLn ""
                                 printNabla nabla
                                 putStrLn ""
                                 printContext c

printResult (Left failure) = putStrLn ("Failue in AVMGU: " ++ show failure)
printResult (Right ((nabla, theta), c)) = do putStrLn "AVMGU result is:"
                                             printTheta theta
                                             printNabla nabla
                                             printContext c

printTheta theta = do putStrLn "Theta:"
                      mapM_ (putStrLn . showSub) theta

printEqs gamma = do putStrLn "Gamma is:"
                    mapM_ (putStrLn . showEq) (toList gamma)

printNabla nabla = do putStrLn "Nabla is:"
                      mapM_ (putStrLn . showCon) nabla

printContext con = do putStrLn "Context is:"
                      print con

showSub (s, e) = show s ++ "->" ++ show e
showEq (e, es, t) = concatMapWith show (\l r -> l ++ " = " ++ r) (e:es)
showCon (Fresh a e) = show a ++ "#" ++ show e
showCon (FreshEq (pi1, a1) (pi2, a2)) = show pi1 ++ " " ++ show a1 ++ " =# " ++ show pi2 ++ " " ++ show a2
showCon (CompFixFc pi1 pi2 s) = let s' = show s in show pi1 ++ " " ++ s' ++ " =# " ++ show pi2 ++ " " ++ s'

concatMapWith f1 f2 ls = foldl1 f2 (map f1 ls)

pureUP :: Either ParseError (LUnificationProblem String, LUnifContext (SwappingList String))
        -> (LUnificationProblem String, LUnifContext (SwappingList String))
pureUP = fromRight

parseUP :: String -> Either ParseError (LUnificationProblem String, LUnifContext (SwappingList String))
parseUP text = fmap (uncurry up) (fromText text)

up eqs nabla = let (eqs0, c0) = compressEquations eqs freshLContext
                   (nablaL, c1) = compressConstraints nabla c0
                   (eqs1, c2) = flattenEqsC eqs0 c1
                   (gamma, c3) = gammaFromList (concatMap fromList eqs1) c2 -- Bug in fromList
                   in ((gamma, nablaL), c3)
