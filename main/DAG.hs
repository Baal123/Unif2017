{-# LANGUAGE FlexibleInstances #-}
module DAG where
import Permutation

type Label = Int

instance PrePermutation Label where
  identity = 0

data Node pi = LabelTerm Label pi
               | Inverse Label Label
               | Concat Label Label Label
               deriving(Show, Eq, Ord)

labelKey (LabelTerm l _) = l
labelKey (Inverse l _) = l
labelKey (Concat l _ _) = l

data PermDAG pi = DAG [Node pi] Int deriving(Show, Eq) -- The Int represent the next label
freshPermDag :: PrePermutation pi => PermDAG pi
freshPermDag = DAG [LabelTerm 0 identity] 1

class Dag dag where
  addComp :: PrePermutation pi => Label -> Label -> dag pi -> (Label, dag pi)
  addInv :: PrePermutation pi => Label -> dag pi -> (Label, dag pi)
  addTerm :: PrePermutation pi => pi -> dag pi -> (Label, dag pi)

instance Dag PermDAG where
  addComp 0 l2 dag = (l2, dag)
  addComp l1 0 dag = (l1, dag)
  addComp l1 l2 (DAG ls nextL) = (nextL, DAG (Concat nextL l1 l2:ls) (nextL + 1))
  addInv 0 dag = (0, dag)
  addInv l1 (DAG ls nextL) = (nextL, DAG (Inverse nextL l1:ls) (nextL + 1))
  addTerm pi1 dag@(DAG ls nextL)
    | pi1 == identity = (0, dag)
    | otherwise = (nextL, DAG (LabelTerm nextL pi1:ls) (nextL + 1))
