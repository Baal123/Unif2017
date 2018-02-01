module UPIO where
import Parser.NLASParser(fromText)
import AVMGU

main :: IO()
main = do putStrLn "Enter Unification Problem:"
          inp <- getLine
          putStrLn "Unification Problem read as:"
          putStrLn inp
          putStrLn "Press any key to end"
          getChar
          putStrLn ""
