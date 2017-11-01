module UnificationProblem where
import           Constraint
import           DAG
import           Equation
import           Expression
import           MMSEquation
import           Substitution
import qualified Data.Map as Map

data Gamma a = Gamma (LEquations a) (LMMSEquations a) deriving(Eq, Show)
toList (Gamma eqs mmsEqs) = eqs ++ toEquations mmsEqs

subToSuspension s (pi, s') (Gamma eqs mmsEqs) lContext
  = let (eqs', lContext') = subWithLabel s (ExpressionSuspension pi s') eqs lContext
        (mmsEqs', lContext'') = subAndMerge s (pi, s') mmsEqs lContext'
        in (Gamma eqs' mmsEqs', lContext'')

type ExSubstitution pi a = (ExpressionVariable a, Expression pi a)
type LExSubstitution a = ExSubstitution Label a
type ExSubstitutions pi a = [ExSubstitution pi a]
type LExSubstitutions a = ExSubstitutions Label a

type LUnificationProblem a = (Gamma a, LConstraints a)

emptyGamma = Gamma [] Map.empty

addToGamma eq (Gamma eqs mmsEqs) lContext
  | mmsEq eq = let (mmsEqs', lContext') = addEquation eq mmsEqs lContext in (Gamma eqs mmsEqs', lContext')
  | otherwise = (Gamma (eq:eqs) mmsEqs, lContext)

addManyToGamma eqs gamma lContext = foldr (\eq (gamma, dag) -> addToGamma eq gamma dag) (gamma, lContext) eqs

addToNabla con (x, y, z) = (x, con:y, z)
addToTheta sub (x, y, z) = (x, y, sub:z)

type SimpleUnificationProblem a = (LEquations a, LConstraints a, LExSubstitutions a)
addToGammaSimple eq (x, y, z) = (eq:x, y, z)
mergeTriple (x1, x2, x3) (y1, y2, y3) = (x1 ++ y1, x2 ++ y2, x3 ++ y3)
