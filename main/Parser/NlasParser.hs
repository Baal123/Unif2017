-----------------------------------------------------------------------------
--
-- Module      :  Parser.NLASParser
-- Copyright   :
-- License     :  BSD3
--
-- Maintainer  :  y_d_k@live.de
-- Stability   :
-- Portability :
--
-- |
--
-----------------------------------------------------------------------------

module Parser.NlasParser (
fromText,
fromUpEntries
) where

import NlasExpression
import qualified Parser.NlasPreParser as Pre
import Unifier
import ConstraintSet
import Equations
import UnificationProblem as UP
import Utilities.CustomUtilities
import Debug.Trace

fromText = fromUpEntries . Pre.parse

fromUpEntries :: [Pre.UpEntry] -> Either Failure (UnificationProblem String)
fromUpEntries upEntries = foldM addToUp upEntries UP.emptyUp

addToUp :: Pre.UpEntry -> UnificationProblem String -> Either Failure (UnificationProblem String)
addToUp (Pre.Fresh a e) (Up gamma uni) = Right (Up gamma (addConToUnifier (toCon (Pre.Fresh a e)) uni))
addToUp (Pre.Eq l r) up = UP.addEquation (toEq (Pre.Eq l r)) up

toCon (Pre.Fresh a e) = Con (toAt a) (toEx e)

toAt (Pre.At a) = AtVar a
toEq (Pre.Eq l r) = Eq (toEx l) (toEx r)

toEx (Pre.Apply swap e) = Apply (toPerm swap) (toEx e)
toEx (Pre.ExAtom a) = produceFromAtom (toAt a)
toEx (Pre.ExVar (Pre.ExN s)) = ExpressionVar (ExVar s)
toEx (Pre.Lam bEx e) = Lam (toBEx bEx) (toEx e)
toEx (Pre.Fn f args) = Fn f (map toEx args)

toBEx (Pre.BExAtom a) = produceFromAtom (toAt a)
toBEx (Pre.BaseApply swap bEx) = addPermutation (toPerm swap) (toBEx bEx)

toPerm (Pre.Swap a b) = [(toAt a, toAt b)]
