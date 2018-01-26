module Main where
import           AVMGUTest          (avmguTests)
import           DecompressionTest  (decomTests)
import           FlatteningTest     (flatteningTests)
import           SubstitutionTest   (substitutionTests)
import           System.Environment (getArgs)

import           Test.Tasty         (defaultMain, testGroup)


main :: IO ()
main = defaultMain $ testGroup "Tests" [flatteningTests, substitutionTests, avmguTests, decomTests]
