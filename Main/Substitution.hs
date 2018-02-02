{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
--{-# LANGUAGE TypeSynonymInstances #-}
module Substitution where
import           Constraint
import           Utilities.CustomUtilities
import           DAG
import           Expression
import           Permutation
import           Equation
import qualified Data.Map as Map

class Substitutable v e c where
  sub :: v  -> e  -> c  -> c

instance Substitutable v e c => Substitutable v e [c] where
  sub s e = map (sub s e)

instance (Permutation pi, Eq a) => Substitutable (ExpressionVariable a) (Expression pi a) (Expression pi a) where
  sub s e ex@(ExpressionSuspension perm s2)
    | s == s2 = addPerm perm e
    | otherwise = ex
  sub s e (Fn f exs) = Fn f (map (sub s e) exs)
  sub s e (Lam b ex) = Lam b (sub s e ex)
  sub s e atSusp = atSusp

instance (Permutation pi, Eq a) => Substitutable (ExpressionVariable a) (Expression pi a) (Constraint pi a) where
  sub s e (Fresh a ex) = Fresh a (sub s e ex)
  sub s e atEq         = atEq

instance (Permutation pi, Eq a) => Substitutable (ExpressionVariable a) (Expression pi a) (Equation pi a) where
  sub s e (e1, es, ClashEq) = (e1, es, ClashEq)
  sub s e (e1, es, ExEq) = let ess'@(e1':es') = map (sub s e) (e1:es)
                               (eq1:eqs) = fromList ess'
                               in case eqs of
                                 [] -> eq1
                                 _ -> (e1', es', SymbolicEq)

-- Add labels
class SubstitutableWithLabel v e c where
  subWithLabel :: (Dag dag, PrePermutation pi) => v -> e -> c -> dag pi -> (c, dag pi)

instance SubstitutableWithLabel v e c => SubstitutableWithLabel v e [c] where
  subWithLabel s e = mapWithContext (subWithLabel s e)

instance Eq a => SubstitutableWithLabel (ExpressionVariable a) (Expression Label a) (Expression Label a) where
  subWithLabel s e ex@(ExpressionSuspension permL s2) dag
    | s == s2 = addPermL permL e dag
    | otherwise = (ex, dag)
  subWithLabel s e (Fn f exs) dag = let (exs', dag') = subWithLabel s e exs dag in (Fn f exs', dag')
  subWithLabel s e (Lam b ex) dag = let (ex', dag') = subWithLabel s e ex dag in (Lam b ex', dag')
  subWithLabel s e atSusp dag = (atSusp, dag)

instance Eq a => SubstitutableWithLabel (ExpressionVariable a) (Expression Label a) (Constraint Label a) where
  subWithLabel s e (Fresh a e1) dag = let (e', dag') = subWithLabel s e e1 dag in (Fresh a e', dag')
  subWithLabel s e con dag = (con, dag)

instance Eq a => SubstitutableWithLabel (ExpressionVariable a) (Expression Label a) (Equation Label a) where
  subWithLabel s e (e1, es, t) dag = let (es', dag') = subWithLabel s e (e1:es) dag
                                         (eq1:eqs) = fromList es'
                                         in case eqs of
                                              [] -> (eq1, dag')
                                              _ -> ((head es', tail es', SymbolicEq), dag')

instance (Ord k, SubstitutableWithLabel v e c) => SubstitutableWithLabel v e (Map.Map k c) where
  subWithLabel s e m dag = let (m', dag') = mapWithContext (subWithLabelOnSnd s e) (Map.toAscList m) dag
                               in (Map.fromAscList m', dag')

subWithLabelOnFst s e (x, y) dag = let (x', dag') = subWithLabel s e x dag in ((x', y), dag')
subWithLabelOnSnd s e (x, y) dag = let (y', dag') = subWithLabel s e y dag in ((x, y'), dag')
