module AVMGU where
import           Constraint
import           DAG
import           Data.List
import qualified Data.Map                  as Map
import           Debug.Trace
import           Equation
import           MMSEquation
import           PermutationInstances
import           ReductionRules
import           UnificationContext
import           UnificationProblem
import           Utilities.CustomUtilities

failure x msg = Left (Fail x msg)

type Result a = (LConstraints a, LExSubstitutions a)
avmgu :: (Show a, Ord a, Eq a) => LUnificationProblem a -> LUnifContext (SwappingList a) -> Either (Failure (LEquations a)) (Result a, LUnifContext (SwappingList a))
avmgu (Gamma uEqs mmsEqs, nabla) lContext
  = let up1 = (uEqs, nabla, [])
        ((uEqs', nabla', theta'), lContext0) = trace ("Work on UEqs: " ++ show uEqs) workOnUEqs up1 lContext
        (mmsEqsFromU, otherEqs) = partition mmsEq (trace ("Partition UEqs result: " ++ show uEqs') uEqs')
        (mmsEqs', lContext1) = trace ("Add to MMS: " ++ show mmsEqsFromU) addEquations mmsEqsFromU mmsEqs lContext0
        in case otherEqs of
           [] -> let (theta'', newEqs, oldEqs, lContext2) = workOnMMsEqs mmsEqs' theta' lContext1
                     (gamma', lContext3) = trace ("Eqs after MMS - (new, old): " ++ show (newEqs, toEquations oldEqs))
                                                 addManyToGamma newEqs (Gamma [] oldEqs) lContext2
                     in case newEqs of
                        [] -> if Map.size oldEqs == 0 then Right ((nabla', theta''), lContext3)
                              else failure (toEquations oldEqs) "MMS failed. Probable cause: Cycle"
                        _ -> let result = trace ("New Gamma: " ++ show gamma') avmgu (gamma', nabla') lContext3
                                 in case result of
                                    Right ((nabla, theta), lContext) -> Right ((nabla, theta ++ theta''), lContext)
                                    Left x -> Left x
           _ -> failure otherEqs "Inconsistency during U-Rules"
