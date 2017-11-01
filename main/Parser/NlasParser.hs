module Parser.NlasParser (
fromText,
fromUpEntries
) where

import Expression
import qualified Parser.NlasPreParser as Pre
import Constraint
import Equation
import Utilities.CustomUtilities
import Debug.Trace
import Permutation
import PermutationInstances

fromText = fromUpEntries . Pre.parse
type EqsAndCons pi a = (Equations pi a, Constraints pi a)

fromUpEntries :: [Pre.UpEntry] -> EqsAndCons (SwappingList String) String
fromUpEntries = foldr addToUp ([], [])

addToUp c@(Pre.Fresh a e) (gamma, nabla) = (gamma, toCon c:nabla)
addToUp eq@(Pre.Eq l r) (gamma, nabla) = (toEq eq:gamma, nabla)

toCon (Pre.Fresh a e) = Fresh (toAt a) (toEx e)

toAt (Pre.At a) = AtVar (Name a)
toEq (Pre.Eq l r) = eq (toEx l) (toEx r)

toEx (Pre.Apply swap e) = addPerm (toPerm swap) (toEx e)
toEx (Pre.ExAtom a) = fromAtVar (toAt a)
toEx (Pre.ExVar (Pre.ExN s)) = fromExVar (ExVar (Name s))
toEx (Pre.Lam bEx e) = Lam (toBEx bEx) (toEx e)
toEx (Pre.Fn f args) = Fn f (map toEx args)


toBEx (Pre.BExAtom a) = (identity, toAt a)
toBEx (Pre.BaseApply swap bEx) = let (pi, a) = toBEx bEx in (add (toPerm swap) pi, a)

toPerm (Pre.Swap a b) = [(toAt a, toAt b)]
