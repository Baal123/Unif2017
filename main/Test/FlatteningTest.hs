{-# LANGUAGE FlexibleInstances #-}
module FlatteningTest (flatteningTests) where
import           DAG
import           Data.List
import           Equation
import           Expression
import           Flattening
import           Test.HUnit              (Assertion, (@?=))
import           Test.Tasty              (TestTree, testGroup)
import           Test.Tasty.HUnit        (testCase)
import           UnificationContext
import           Utilities.TestUtilities

-- Tests
-- Right part of the equation is the expected value
s1 = ExVar (Name "S1")
s2 = ExVar (Name "S2")
s = ExVar (Name "S")
a = AtVar (Name "a")
b = AtVar (Name "b")

exFromInt :: Int -> ExpressionVariable String
exFromInt a = ExVar (TmpName a)

eqs1 = [[Fn "f" [Lam (2 :: Label, a) (fromExVar s1)], Fn "f" [Lam (3, b) (fromExVar s2)]]]
eqs1' = [[Fn "f" [fromExVar s], Fn "f" [fromExVar s]],
        [fromExVar s, Lam (0 :: Label, a) (fromExVar s1)],
        [fromExVar s, Lam (0, a) (fromExVar s2)]]

testEquation11 = sameEqsStructure eqs1' (fst (flattenEqsC eqs1 freshContext)) @?= True
testEquation12 = (fst . freshExVar . snd) (flattenEqsC eqs1 freshContext) @?= exFromInt 2

eqs2 = [[Fn "f" [Fn "a" [], Fn "a" [], Fn "g" [fromAtVar a]],
         Fn "f" [Fn "a" [], Fn "a" [], Fn "g" [Fn "g"
                                                [Lam (0 :: Label, a) (fromExVar s1)]]]]]
eqs2' = [[Fn "f" [fromExVar s, fromExVar s, fromExVar s],
          Fn "f" [fromExVar s, fromExVar s, fromExVar s]],
         [fromExVar s, Fn "a" []], [fromExVar s, Fn "a" []], [fromExVar s, Fn "g" [fromAtVar a]],
         [fromExVar s, Fn "a" []], [fromExVar s, Fn "a" []],
         [fromExVar s, Fn "g" [fromExVar s]], [fromExVar s, Fn "g" [fromExVar s]],
         [fromExVar s, Lam (0 :: Label, a) (fromExVar s1)]]

testEquation21 = sameEqsStructure eqs2' (fst (flattenEqsC eqs2 freshContext)) @?= True
testEquation22 = (fst . freshExVar . snd) (flattenEqsC eqs2 freshContext) @?= exFromInt 8

testAtomFlattening1 = all isAtFlattened (concat eqs1) @?= False
testAtomFlattening2 = let flattenedTriple = map (`atFlatten` freshContext) (concat eqs1)
                          flattenedEs = map (\(x, y, z) -> x) flattenedTriple
                          in all isAtFlattened flattenedEs @?= True

flatteningTests = testGroup "FlatteningTestGroup"
                  [testCase "flattenLambdaEquations1" testEquation11,
                   testCase "flattenLambdaEquations2" testEquation12,
                   testCase "flattenFunctionEqsEquations1" testEquation21,
                   testCase "flattenFunctionEqsEquations2" testEquation22,
                   testCase "testAtomFlattening1" testAtomFlattening1,
                   testCase "testAtomFlattening2" testAtomFlattening2]
