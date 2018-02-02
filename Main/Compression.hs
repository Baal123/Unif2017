module Compression where
import           Constraint
import           DAG
import           Equation
import           Expression
import           Permutation
import           Utilities.CustomUtilities
-- Expressions
compressExpression :: (Dag dag, PrePermutation pi) => Expression pi a -> dag pi -> (LExpression a, dag pi)
compressExpression (AtomSuspension pi a) dag = let (l, dag') = addTerm pi dag
                                                   in (AtomSuspension l a, dag')

compressExpression (ExpressionSuspension pi s) dag = let (l, dag') = addTerm pi dag
                                                         in (ExpressionSuspension l s, dag')

compressExpression (Fn f es) dag = let (es', dag') = compressExpressions es dag
                                       in (Fn f es', dag')

compressExpression (Lam (pi, a) e) dag = let (l, dag') = addTerm pi dag
                                             (e', dag'') = compressExpression e dag'
                                             in (Lam (l, a) e', dag'')

compressExpressions :: (Dag dag, PrePermutation pi) => [Expression pi a] -> dag pi -> ([LExpression a], dag pi)
compressExpressions = mapWithContext compressExpression

-- Constraints
compressConstraint :: (Dag dag, PrePermutation pi) => Constraint pi a -> dag pi -> (LConstraint a, dag pi)
compressConstraint (Fresh a e) dag = let (e', dag') = compressExpression e dag
                                         in (Fresh a e', dag')

compressConstraint (FreshEq (pi1, a1) (pi2, a2)) dag = let (l1, dag1) = addTerm pi1 dag
                                                           (l2, dag') = addTerm pi2 dag1
                                                           in (FreshEq (l1, a1) (l2, a2), dag')

compressConstraint (CompFixFc pi1 pi2 s) dag = let (l1, dag1) = addTerm pi1 dag
                                                   (l2, dag') = addTerm pi2 dag1
                                                   in (CompFixFc l1 l2 s, dag')

compressConstraints :: (Dag dag, PrePermutation pi) => Constraints pi a -> dag pi -> (LConstraints a, dag pi)
compressConstraints = mapWithContext compressConstraint

-- Equations
compressEquations eqs dag = mapWithContext compressExpressions eqs dag
