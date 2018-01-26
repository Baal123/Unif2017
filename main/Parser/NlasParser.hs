module Parser.NlasParser (
fromText,
fromEntries,
upFromText,
upFromEntries
) where

import           Compression
import           Constraint
import           Debug.Trace
import           Equation
import           Expression
import           Flattening
import qualified Parser.NlasPreParser      as Pre
import           Permutation
import           PermutationInstances
import           UnificationContext
import           UnificationProblem
import           Utilities.CustomUtilities

fromText = fromEntries . Pre.parse
type EqsAndCons pi a = ([Expressions pi a], Constraints pi a)

fromEntries :: [Pre.UpEntry] -> EqsAndCons (SwappingList String) String
fromEntries = foldr addToUp ([], [])

upFromText = upFromEntries . Pre.parse
upFromEntries :: [Pre.UpEntry] -> (LUnificationProblem String, LUnifContext (SwappingList String))
upFromEntries es = let (gamma, nabla) = fromEntries es
                       (gamma0, c0) = uncurry compressEquations . uncurry flattenEqsC $ (gamma, freshLContext)
                       (gamma', c1) = gammaFromList (concatMap fromList gamma0) c0
                       (nabla', c') = compressConstraints nabla c1
                       in ((gamma', nabla'), c')

addToUp c@(Pre.Fresh a e) (gamma, nabla) = (gamma, toCon c:nabla)
addToUp eq@(Pre.Eq l r) (gamma, nabla)   = (toEq eq:gamma, nabla)

toCon (Pre.Fresh a e) = Fresh (toAt a) (toEx e)

toAt (Pre.At a) = AtVar (Name a)
toEq (Pre.Eq l r) = [toEx l, toEx r]
--toEq (Pre.Eq l r) = eq (toEx l) (toEx r)

toEx (Pre.Apply swap e)      = addPerm (toPerm swap) (toEx e)
toEx (Pre.ExAtom a)          = fromAtVar (toAt a)
toEx (Pre.ExVar (Pre.ExN s)) = fromExVar (ExVar (Name s))
toEx (Pre.Lam bEx e)         = Lam (toBEx bEx) (toEx e)
toEx (Pre.Fn f args)         = Fn f (map toEx args)


toBEx (Pre.BExAtom a) = (identity, toAt a)
toBEx (Pre.BaseApply swap bEx) = let (pi, a) = toBEx bEx in (add (toPerm swap) pi, a)

toPerm (Pre.Swap a b) = [(toAt a, toAt b)]
