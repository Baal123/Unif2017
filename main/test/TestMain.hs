module Main where
import System.Environment (getArgs)
import FlatteningTest (flatteningTests)
import SubstitutionTest (substitutionTests)
import AVMGUTest (avmguTests)
import DecompressionTest (decomTests)

import Test.Tasty (defaultMain, testGroup)


main :: IO ()
main = defaultMain $ testGroup "Tests" [flatteningTests, substitutionTests, avmguTests, decomTests]
