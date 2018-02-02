module Utilities.TestUtilities where
import           Data.List
import           Expression
import           Permutation
import           Flattening
import           Equation
import           UnificationContext
import           Utilities.CustomUtilities

exVar s = ExVar (Name s)
atVar a = AtVar (Name a)

sameExNames (ExVar (TmpName a)) (ExVar (TmpName b)) = a == b
sameAtName (AtVar (TmpName a)) (AtVar (TmpName b)) = a == b

sameStructure (Fn f args) (Fn g args')
  | f == g = let res = zipWith sameStructure args args' in and  res
  | otherwise = False

sameStructure (Lam a e) (Lam b e') = sameStructure e e'
sameStructure (ExpressionSuspension perm s) (ExpressionSuspension perm' s') = True
sameStructure (AtomSuspension perm a) (AtomSuspension perm' b) = True
sameStructure e1 e2 = False

sameEqStructure [] es2 = null es2
sameEqStructure es1@(e:es) es2
  = let (similarEs1, differentEs1) = partition (sameStructure e) es1
        (similarEs2, differentEs2) = partition (sameStructure e) es2 in
          length similarEs1 == length similarEs2 && sameEqStructure differentEs1 differentEs2

sameEqsStructure [] eqs2 = null eqs2
sameEqsStructure eqs1@(eq:eqs) eqs2
  = let (sEqs1, dEqs1) = partition (sameEqStructure eq) eqs1
        (sEqs2, dEqs2) = partition (sameEqStructure eq) eqs2 in
          length sEqs1 == length sEqs2 && sameEqsStructure dEqs1 dEqs2

atFlatten :: (Eq pi, PrePermutation pi, UnificationContext c) => Expression pi a -> c -> (Expression pi a, Equations pi a, c)
atFlatten (Lam (pi, a) e) c = let (f, eq, c') = flattenAtSusp (pi, a) c
                                  (e', eqs, c'') = atFlatten e c'
                                  in (Lam (identity, f) e', eq:eqs, c'')
atFlatten (Fn f args) c
  = let (es, eqs, c') = foldr (\e (es, eqs, c) -> let (e', eqs', c') = atFlatten e c in (e':es, eqs' ++ eqs, c')) ([], [], c) args
        in (Fn f (reverse es), eqs, c')
atFlatten (AtomSuspension pi a) c = let (f, eq, c') = flattenAtSusp (pi, a) c
                                        in (fromAtVar f, [eq], c')
atFlatten e c = (e, [], c)

isAtFlattened (Lam (pi, a) e) = pi == identity && isAtFlattened e
isAtFlattened (Fn f es) = all isAtFlattened es
isAtFlattened (AtomSuspension pi a) = pi == identity
isAtFlattened _ = True
