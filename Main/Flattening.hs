module Flattening
      (flattenExsC,
       flattenEqsC,
       flattenAtSusp) where
import           Equation
import           Expression
import           Permutation
import           UnificationContext
import           Utilities.CustomUtilities

-- Flattening without context
-- Goal: flatten compound expressions to depth 1
flattenEx1 e s = let es = subExpressions e
                     (es', eqs, s') = flattenExs0 es s -- flatten subExpressions to depth 0
                     con = constructor e in (con es', eqs, s') -- reconstruct the expression with flattened subExpressions

-- Add an equation at s = flattened e and equations for "flattened e"
flattenEx0 e s
  | isSuspension e = (e, [], s)
  | otherwise = let (e', eqs, s') = flattenEx1 e (next s) in (e', [fromExVar s, e']:eqs, s')

-- Iterate through expressions
flattenExs0 es s = let (esF, eqs, s') = foldr g ([], [], s) es in (esF, eqs, s')
                   where
                     g e (es, eqs, s) = let (e', eqs', s') = flattenEx0 e s in (e':es, eqs ++ eqs', s')

-- main flattening functions
flattenExs :: PrePermutation pi => [Expression pi a] -> ExpressionVariable a -> ([Expression pi a], [[Expression pi a]], ExpressionVariable a)
flattenExs es s = let (esF, eqs, s') = foldr g ([], [], s) es  in (esF, eqs, s')
                  where
                    g e (es, eqs, s) = let (e', eqs', s') = flattenEx1 e s in (e':es, eqs ++ eqs', s')

flattenEqs :: PrePermutation pi => [[Expression pi a]]-> ExpressionVariable a -> ([[Expression pi a]], ExpressionVariable a)
flattenEqs eqs s = let (eqs', s') = foldr g ([], s) eqs  in (eqs', s')
                   where
                     g eq (eqs, s) = let (eq', eqs', s') = flattenExs eq s in (eq':eqs ++ eqs', s')

------------------------
-- Flatten with context
---
flattenExC1 :: (PrePermutation pi, UnificationContext c) => Expression pi a -> c -> (Expression pi a, [[Expression pi a]], c)
flattenExC1 e context = let es = subExpressions e
                            (es', eqs, context') = flattenExsC0 es context -- flatten subExpressions to depth 0
                            con = constructor e in (con es', eqs, context') -- reconstruct the expression with flattened subExpressions

-- Add an equation at s = flattened e and equations for "flattened e"
flattenExC0 :: (PrePermutation pi, UnificationContext c) => Expression pi a -> c -> (Expression pi a, [[Expression pi a]], c)
flattenExC0 e context
  | isSuspension e = (e, [], context)
  | otherwise = let (s, context') = freshExVar context
                    (e', eqs, context'') = flattenExC1 e context' in (fromExVar s, [fromExVar s, e']:eqs, context'')

-- Iterate through expressions
flattenExsC0 es context = let (esF, eqs, context') = foldr g ([], [], context) es in (esF, eqs, context') --iteration reverses order of es
                          where
                            g e (es, eqs, context) = let (e', eqs', context') = flattenExC0 e context in (e':es, eqs ++ eqs', context')

-- main flattening functions
flattenExsC :: (PrePermutation pi, UnificationContext c) => [Expression pi a] -> c -> ([Expression pi a], [[Expression pi a]], c)
flattenExsC es context = let (esF, eqs, context') = foldr g ([], [], context) es in (esF, eqs, context') --iteration reverses order of es, b
                         where
                           g e (es, eqs, context) = let (e', eqs', context') = flattenExC1 e context in (e':es, eqs ++ eqs', context')

flattenEqsC :: (PrePermutation pi, UnificationContext c) => [[Expression pi a]] -> c -> ([[Expression pi a]], c)
flattenEqsC eqs context = let (eqs', context') = foldr g ([], context) eqs in (eqs', context')
                          where
                            g eq (eqs, context) = let (eq', eqs', context') = flattenExsC eq context in (eq':eqs ++ eqs', context')

flattenAtSusp :: (PrePermutation pi, Eq pi, UnificationContext c) => (pi, AtomVariable a) -> c -> (AtomVariable a, Equation pi a, c)
flattenAtSusp (pi, a) c
  | pi == identity = (a, (fromAtVar a, [], AtEq), c)
  | otherwise = let (f, c') = freshAtVar c in (f, (fromAtVar f, [fromBaseTuple (pi, a)], AtEq), c')
