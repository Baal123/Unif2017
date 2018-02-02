{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module UnificationContext where
import           Constraint
import           Utilities.CustomUtilities
import           DAG
import           Data.List
import           Expression
import           Permutation

class UnificationContext c where
  freshExVar :: c -> (ExpressionVariable a, c)
  freshAtVar :: c -> (AtomVariable a, c)

data UnifContext = UnifCon Int Int deriving(Show, Eq)

freshContext = UnifCon 0 0

instance UnificationContext UnifContext where
  freshExVar (UnifCon a s) = (ExVar (TmpName s), UnifCon a (s + 1))
  freshAtVar (UnifCon a s) = (AtVar (TmpName a), UnifCon (a + 1) s)

instance UnificationContext c => UnificationContext (c, a) where
  freshExVar (c, dag) = let (x, c') = freshExVar c in (x, (c', dag))
  freshAtVar (c, dag) = let (x, c') = freshAtVar c in (x, (c', dag))

newtype LUnifContext pi = LC (UnifContext, PermDAG pi) deriving(UnificationContext, Eq, Show)

freshLContext :: PrePermutation pi => LUnifContext pi
freshLContext = LC (freshContext, freshPermDag)

instance Dag LUnifContext where
  addComp l1 l2 (LC (con, dag)) = let (l3, dag') = addComp l1 l2 dag in (l3, LC (con, dag'))
  addInv l1 (LC (con, dag)) = let (l2, dag') = addInv l1 dag in (l2, LC (con, dag'))
  addTerm pi (LC (con, dag)) = let (l, dag') = addTerm pi dag in (l, LC (con, dag'))
