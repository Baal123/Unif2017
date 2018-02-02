{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeSynonymInstances #-}
module Expression where
import           DAG
import           Data.List
import           Data.Maybe
import           Permutation
import           Utilities.CustomUtilities

-- Essential definitions
data VariableName a = Name a | TmpName Int deriving(Eq, Show, Functor)
instance Ord a => Ord (VariableName a) where
  Name a <= Name b = a <= b
  Name a <= TmpName x = True
  TmpName x <= TmpName y = x <= y

class Next c where
  next :: c -> c

instance Next (VariableName a) where
  next (Name x) = TmpName 0
  next (TmpName x) = TmpName (x + 1)

newtype AtomVariable a = AtVar (VariableName a) deriving(Ord, Eq, Show, Functor, Next)
newtype ExpressionVariable a = ExVar (VariableName a) deriving(Ord, Eq, Show, Functor, Next)

data Expression pi a = AtomSuspension pi (AtomVariable a)
                       | ExpressionSuspension pi (ExpressionVariable a)
                       | Fn String [Expression pi a]
                       | Lam (pi, AtomVariable a) (Expression pi a)
                       deriving(Ord, Eq, Show)
type Expressions pi a = [Expression pi a]

instance Functor (Expression pi) where
  fmap f (AtomSuspension pi a) = AtomSuspension pi (fmap f a)
  fmap f (ExpressionSuspension pi s) = ExpressionSuspension pi (fmap f s)
  fmap f (Fn g es) = Fn g (map (fmap f) es)
  fmap f (Lam (pi, a) e) = Lam (pi, fmap f a) (fmap f e)

instance Functor2 Expression where
  fmap2 f (AtomSuspension pi a) = AtomSuspension (f pi) a
  fmap2 f (ExpressionSuspension pi s) = ExpressionSuspension (f pi) s
  fmap2 f (Fn g es) = Fn g (map (fmap2 f) es)
  fmap2 f (Lam (pi, a) e) = Lam (f pi, a) (fmap2 f e)

exVars (Fn _ args) = concatMap exVars args
exVars (Lam _ e) = exVars e
exVars (ExpressionSuspension _ s) = [s]
exVars _ = []

data ExpressionType = AtSusp | ExSusp | Fun String | Lambda deriving(Ord, Show, Eq)

eType (AtomSuspension _ _)       = AtSusp
eType (ExpressionSuspension _ _) = ExSusp
eType (Fn f _)                   = Fun f
eType (Lam _ _)                  = Lambda

-- constructor . subExpressions == id
constructor (Fn f es) = Fn f
constructor (Lam a e) = Lam a . head
constructor e         = const e

subExpressions (Lam a e) = [e]
subExpressions (Fn f es) = es
subExpressions susp      = []

isAtSuspension (AtomSuspension pi a) = True
isAtSuspension e                     = False

isExSuspension (ExpressionSuspension pi s) = True
isExSuspension e                           = False

isSuspension e = isAtSuspension e || isExSuspension e

isCompund = not . isSuspension

fromAtVar :: PrePermutation pi => AtomVariable a -> Expression pi a
fromAtVar = AtomSuspension identity

fromExVar :: PrePermutation pi => ExpressionVariable a -> Expression pi a
fromExVar = ExpressionSuspension identity

class Atomizable c where
  getAtoms :: c a -> [AtomVariable a]

fromBaseTuple (pi1, a) = AtomSuspension pi1 a
toBaseTuple (AtomSuspension pi1 a) = (pi1, a)

addPerm perm (Fn f es) = Fn f (map (addPerm perm) es)
addPerm perm (Lam (perm', a) e) = Lam (add perm perm', a) (addPerm perm e)
addPerm perm (ExpressionSuspension perm' s) = ExpressionSuspension (add perm perm') s
addPerm perm (AtomSuspension perm' a) = AtomSuspension (add perm perm') a

addPermToEs perm = map (addPerm perm)
-- Labeled version with DAG
type LExpression a = Expression Label a
type LExpressions a = [LExpression a]

addPermL l1 (AtomSuspension l2 a) dag = (AtomSuspension lN a, dagN)
                                        where
                                          (lN, dagN) = addComp l1 l2 dag

addPermL l1 (ExpressionSuspension l2 e) dag = (ExpressionSuspension lN e, dagN)
                                        where
                                          (lN, dagN) = addComp l1 l2 dag

addPermL l1 (Fn f es) dag = let (es', dag') = addPermToLes l1 es dag in (Fn f es', dag')

addPermL l1 (Lam b e) dag = (Lam (toBaseTuple b') e', dag'')
                            where
                              (b', dag') = addPermL l1 (fromBaseTuple b) dag
                              (e', dag'') = addPermL l1 e dag'

addPermToLes l = mapWithContext (addPermL l)
