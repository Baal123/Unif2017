{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE TypeSynonymInstances #-}
module Display where
import           Constraint
import           DAG
import           Data.List
import           Equation
import           Expression
import           Permutation
import           PermutationInstances
import           UnificationContext
import           UnificationProblem
import           Utilities.CustomUtilities

class Displayable c where
  display :: c -> String

instance Displayable String where
  display = id

instance (PrePermutation pi, Displayable pi, Displayable a) => Displayable (Expression pi a) where
  display (AtomSuspension pi a)
   | pi /= identity = display pi ++ " " ++ display a
   | otherwise = display a
  display (ExpressionSuspension pi s)
    | pi /= identity = display pi ++ " " ++ display s
    | otherwise = display s
  display (Fn f es) = f ++ " " ++ concatMapWith (\l r -> l ++ " " ++ r) displayArg es
                      where
                        displayArg (Fn f []) = f
                        displayArg e
                          | isSuspension e = display e
                          | otherwise = "(" ++ display e ++ ")"
  display (Lam aEx e') = "\\" ++ display (fromBaseTuple aEx) ++ " -> " ++ display e'

instance Displayable a => Displayable (VariableName a) where
  display (Name a)    = display a
  display (TmpName a) = show a

instance Displayable a => Displayable (AtomVariable a) where
  display (AtVar n@(TmpName _)) = "_f" ++ display n
  display (AtVar n@(Name _))    = display n

instance Displayable a => Displayable (ExpressionVariable a) where
  display (ExVar n@(TmpName _)) = "_F" ++ display n
  display (ExVar n@(Name _))    = display n

instance Displayable Label where
  display = show

instance Displayable a => Displayable (Swapping a) where
  display (a,b) = "(" ++ display a ++ " " ++ display b ++ ")"

instance Displayable a => Displayable (SwappingList a) where
  display [] = "[]"
  display ls = concatMapWith (\l r -> l ++ " " ++ r) display ls

instance (PrePermutation pi, Displayable pi, Displayable a) => Displayable (Constraint pi a) where
  display (Fresh a e) = display a ++ " # " ++ display e
  display (FreshEq a1 a2) = display (fromBaseTuple a1) ++ " =# " ++ display (fromBaseTuple a2)
  display (CompFixFc pi1 pi2 s) = "CompFixFc(" ++ display pi1 ++ ", " ++  display pi2 ++ ", " ++ display s ++ ")"

instance (PrePermutation pi, Displayable pi, Displayable a) => Displayable (Constraints pi a) where
  display = concatMapWith (\l r -> l ++ "\n" ++ r) display

instance (PrePermutation pi, Displayable pi, Displayable a) => Displayable (Equation pi a) where
  display (e, es, t) = concatMapWith (\l r -> l ++ " = " ++ r) display (e:es)

instance (PrePermutation pi, Displayable pi, Displayable a) => Displayable (Equations pi a) where
  display = concatMapWith (\l r -> l ++ "\n" ++ r) display

instance Displayable pi => Displayable (Node pi) where
  display (LabelTerm l pi) = display l ++ " -> " ++ display pi
  display (Inverse l1 l2) = display l1 ++ " -> " ++ display l2 ++ "^-1"
  display (Concat l1 l2 l3) = display l1 ++ " -> " ++ display l2 ++ " * " ++ display l3

instance Displayable pi => Displayable (PermDAG pi) where
  display (DAG ls _) = concatMapWith (\l r -> l ++ "\n" ++ r) display ls

instance Displayable pi => Displayable (LUnifContext pi) where
   display (LC (_, dag)) = display dag

instance (PrePermutation pi, Displayable pi, Displayable a) => Displayable (ExSubstitution pi a) where
  display (s, e) = display s ++ " -> " ++ display e

instance (PrePermutation pi, Displayable pi, Displayable a) => Displayable (ExSubstitutions pi a) where
  display = concatMapWith (\l r -> l ++ "\n" ++ r) display
