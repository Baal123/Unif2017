{-# LANGUAGE FlexibleInstances #-}
module SubstitutionTest(substitutionTests) where
import           Constraint
import           DAG
import           Data.List
import           Equation
import           Expression
import           Permutation
import           Substitution
import           Test.HUnit              (Assertion, (@?=))
import           Test.Tasty              (TestTree, testGroup)
import           Test.Tasty.HUnit        (testCase)
import           UnificationContext
import           Utilities.TestUtilities
import           PermutationInstances

-- no label
pi1 = [1, 2] :: [Int]
pi2 = [2, 3] :: [Int]
e1 = Lam ([], atVar "a") (Fn "f" [ExpressionSuspension pi1 (exVar "s"), Fn "a" []])
e2 = ExpressionSuspension pi2 (exVar "s2")
e3 = Lam ([], atVar "a") (Fn "f" [ExpressionSuspension (pi1 `add` pi2) (exVar "s2"), Fn "a" []])
subTest1 = sub (exVar "s") e2 e1 @?= e3

-- labeled setting
pi1L = 1 :: Label
pi2L = 2 :: Label
e1L = Lam (0, atVar "a") (Fn "f" [ExpressionSuspension pi1L (exVar "s"), Fn "a" []])
e2L = ExpressionSuspension pi2L (exVar "s2")

dag = DAG [LabelTerm pi1L pi1, LabelTerm pi2L pi2, LabelTerm 0 []] 3
(n, dag1) = addComp 1 2 dag
e3L = Lam (0, atVar "a") (Fn "f" [ExpressionSuspension n (exVar "s2"), Fn "a" []])

lSubTest1 = subWithLabel (exVar "s") e2L e1L dag @?= (e3L, dag1)
substitutionTests = testGroup "SubstitutionTestGroup"
                                  [testCase "subTest1" subTest1
                                  ,testCase "lSubTest1" lSubTest1]
