{-# LANGUAGE FlexibleInstances #-}
module AVMGUTest where
import           Constraint
import           DAG
import           Data.List
import           Equation
import           Expression
import           Permutation
import           Test.HUnit              (Assertion, (@?=))
import           Test.Tasty              (TestTree, testGroup)
import           Test.Tasty.HUnit        (testCase)
import           UnificationContext
import           Utilities.TestUtilities
import           Utilities.CustomUtilities
import           UnificationProblem
import           MMSEquation
import           ReductionRules
import           AVMGU
import           Data.List.Unique
import           Control.Arrow((***), first)
import           Decompression
import qualified Data.Map as Map
import           Data.Map((!))
import           Debug.NoTrace
import           Utilities.TestVars
import           PermutationInstances

dag (LC x) = snd x

buildProblem1 :: (Ord a, Show a) => LEquations a -> (LUnificationProblem a, LUnifContext (SwappingList String))
buildProblem1 eqs = let (gamma, lContext) = addManyToGamma eqs emptyGamma freshLContext
                        in ((gamma, []), lContext)

eqF :: ExpressionVariable a -> ExpressionVariable a -> LEquation a
eqF s1 s2 = (fromExVar s1, [Fn "f" [fromExVar s2, fromExVar s2]], MMS)

fSub s1 s2 = (s1, Fn "f" [fromExVar s2, fromExVar s2])

up11 = buildProblem1 [eqF s1 s2, eqF s2 s3, eqF s3 s4]
up12 = buildProblem1 [eqF s1 s2, eqF s3 s4, eqF s2 s3]
up13 = buildProblem1 [eqF s2 s3, eqF s1 s2, eqF s3 s4]
up14 = buildProblem1 [eqF s2 s3, eqF s3 s4, eqF s1 s2]
up15 = buildProblem1 [eqF s3 s4, eqF s2 s3, eqF s1 s2]
up16 = buildProblem1 [eqF s3 s4, eqF s1 s2, eqF s2 s3]

solution1 :: Result String
solution1 = ([], [fSub s3 s4, fSub s2 s3, fSub s1 s2])

up2 = buildProblem1 [(fromExVar s1, [Fn "f" [fromExVar s3, fromExVar s2],
                                     Fn "f" [fromExVar s2, fromExVar s3],
                                     Fn "f" [fromExVar s3, fromExVar s4]], MMS)]

solution2 :: Result String
solution2 = ([CompFixFc 0 0 s3], sortUniq [(s1, Fn "f" [fromExVar s3, fromExVar s2]), (s2, fromExVar s3), (s4, fromExVar s3)])

up3 = buildProblem1 [(Lam (0, a) (fromAtVar b), [Lam (0, c) (fromAtVar d)], LambdaEq)]
solution3 = [FreshEq (identity, b) ([(a, c)], d), Fresh a (Lam (identity, c) (fromAtVar d))]

-- Tests
-- Right part of the equation is the expected value
testMMS11 = uncurry avmgu up11 @?= Right (solution1, freshLContext)
testMMS12 = uncurry avmgu up12 @?= Right (solution1, freshLContext)
testMMS13 = uncurry avmgu up13 @?= Right (solution1, freshLContext)
testMMS14 = uncurry avmgu up14 @?= Right (solution1, freshLContext)
testMMS15 = uncurry avmgu up15 @?= Right (solution1, freshLContext)
testMMS16 = uncurry avmgu up16 @?= Right (solution1, freshLContext)
mmsTests1 = [testCase "testMMS1" testMMS11, testCase "testMMS1" testMMS12, testCase "testMMS1" testMMS13,
             testCase "testMMS1" testMMS14, testCase "testMMS1" testMMS15, testCase "testMMS1" testMMS16]

testMMS2 = fmap (first (sortUniq *** sortUniq)) (uncurry avmgu up2) @?= Right (solution2, freshLContext)

decomNabla ((nabla, _), context) = let lMap = decomPermDag (dag context)
                                        in trace ("Decom nabla: " ++ show lMap) map (fmap2 (lMap !)) nabla

testLam1 = let result = uncurry avmgu up3
               in fmap decomNabla result @?= Right solution3

avmguTests = testGroup "AvMguTestGroup"
                                  (mmsTests1 ++
                                   [testCase "testMMS2" testMMS2])

                                  --  testCase "testLam1" testLam1
