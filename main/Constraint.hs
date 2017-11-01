module Constraint where
import           Utilities.CustomUtilities
import           Expression
import           Permutation
import           DAG

data Constraint pi a = Fresh (AtomVariable a) (Expression pi a)       -- a # e
                     | FreshEq (pi, AtomVariable a) (pi, AtomVariable a)  -- pi1 a =_# pi2 b
                     | CompFixFc pi pi (ExpressionVariable a)            -- pi_1 s = pi_2 s
                     deriving (Eq, Ord, Show)

instance Functor2 Constraint where
  fmap2 f (Fresh a e) = Fresh a (fmap2 f e)
  fmap2 f (FreshEq (pi1, a) (pi2, b)) = FreshEq (f pi1, a) (f pi2, b)
  fmap2 f (CompFixFc pi1 pi2 s) = CompFixFc (f pi1) (f pi2) s

instance Functor (Constraint pi) where
  fmap f (Fresh a e) = Fresh (fmap f a) (fmap f e)
  fmap f (FreshEq (pi1, a) (pi2, b)) = FreshEq (pi1, fmap f a) (pi2, fmap f b)
  fmap f (CompFixFc pi1 pi2 s) = CompFixFc pi1 pi2 (fmap f s)

type LConstraint a = Constraint Label a

type Constraints pi a = [Constraint pi a]
type LConstraints a = Constraints Label a
