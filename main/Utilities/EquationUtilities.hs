module Utilities.EquationUtilities where
import           DAG
import           Equation
import           Expression
import           Permutation

movePermToRight :: (PrePermutation pi, Dag dag) => LEquation a -> dag pi -> (LEquation a, dag pi)
movePermToRight (ExpressionSuspension pi s, es, t) lContext = let (pi', lContext0) = addInv pi lContext
                                                                  (es', lContext') = addPermToLes pi' es lContext0
                                                                  in ((ExpressionSuspension identity s, es', t), lContext')

movePermToRight (AtomSuspension pi a, es, t) lContext = let (pi', lContext0) = addInv pi lContext
                                                            (es', lContext') = addPermToLes pi' es lContext0
                                                            in ((AtomSuspension identity a, es', t), lContext')

movePermToRight e lContext = (e, lContext)
