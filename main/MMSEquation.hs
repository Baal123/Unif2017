{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module MMSEquation
        (LMMSEquationValue,
         LMMSEquations,
         subAndMerge,
         popEquation,
         topEquations,
         popTopEquations,
         toEquations,
         addEquation,
         addEquations,
         toSubsAndEqs) where
import           DAG
import           Data.Map                  (Map, adjust, delete, (!), fromListWith, toList, elems)
import qualified Data.Map                  as Map
import           Equation
import           Expression
import           Utilities.CustomUtilities
import           Permutation
import           Utilities.EquationUtilities
import           Substitution
import Debug.NoTrace

type LMMSEquationValue a = ([LExpression a], Int)

instance Eq a => SubstitutableWithLabel (ExpressionVariable a) (LExpression a) (LMMSEquationValue a) where
  subWithLabel s e (es, c) dag = let (es', dag') = subWithLabel s e es dag in ((es', c), dag')

incr (e, c) = (e, c + 1)
mergeMMsValues (es1, c1) (es2, c2) = (es1 ++ es2, c1 + c2)

type LMMSEquations a = Map (ExpressionVariable a) (LMMSEquationValue a)

exVarsOnRightSide1 = concatMap (\(_, es, _) -> concatMap exVars es)
exVarsOnRightSide2 eqs = concatMap (\(es, _) -> concatMap exVars es) (elems eqs)
countS s eqs = length (filter (== s) (exVarsOnRightSide2 eqs))

-- O(n)
subAndMerge :: (Show a, Dag dag, Ord a) => ExpressionVariable a -> (Label, ExpressionVariable a) -> LMMSEquations a -> dag pi -> (LMMSEquations a, dag pi)
subAndMerge s (pi, s') eqs lContext
  = let (ess, cs) = Map.findWithDefault ([], countS s eqs) s eqs
        (pi', lContext') = addInv pi lContext
        (vs, lContext'') = addPermToLes pi' ess lContext'
        eqs' = delete s eqs
        mEqs = adjust (mergeMMsValues (vs, cs)) s' eqs' -- eqs merged,
        in subWithLabel s (ExpressionSuspension pi s') mEqs lContext''

-- O(n log(n)), n == number of variables
fromEquations :: (Ord a, Dag dag) => LEquations a -> dag pi -> (LMMSEquations a, dag pi)
fromEquations eqs lContext = let (eqs0, lContext') = mapWithContext movePermToRight eqs lContext -- O(n)
                                 mEqs = fromListWith mergeMMsValues $ map (\(ExpressionSuspension _ s, es, _) -> (s, (es, 0))) eqs0 -- O(n log(n))
                                 eqs'' = foldr (adjust incr) mEqs (exVarsOnRightSide1 eqs)  -- O(n log(n))
                                 in (eqs'', lContext')

-- O(n)
addEquation :: (Show a, Ord a, Dag dag) => LEquation a -> LMMSEquations a -> dag pi -> (LMMSEquations a, dag pi)
addEquation eq eqs dag = let (eq', dag') = movePermToRight eq dag
                             (ExpressionSuspension _ s, es, _) = eq
                             eqs' = foldr (adjust incr) eqs (concatMap exVars es)
                             c = countS s eqs
                             in trace ("Add eq with key to keys: " ++ show (s, Map.keys eqs))
                                      (Map.insertWith mergeMMsValues s (es, c) eqs', dag')

addEquations eqs mmsEqs dag = foldr (\eq (eqs, dag) -> addEquation eq eqs dag) (mmsEqs, dag) eqs

toEquations :: LMMSEquations a -> LEquations a
toEquations eqs = map (\(s, (es, c)) -> (fromExVar s, es, MMS)) (toList eqs)

toSubsAndEqs :: LMMSEquations a -> ([(ExpressionVariable a, LExpression a)], LEquations a)
toSubsAndEqs eqs = let g e = case eType e of
                                  Lambda -> LambdaEq
                                  Fun f  -> FEq f
                      in unzip $ map (\(s, (es, c)) -> ((s, head es),(head es, tail es, g (head es)))) (toList eqs)

popEquation :: Ord a => ExpressionVariable a -> LMMSEquations a -> (LMMSEquationValue a, LMMSEquations a)
popEquation s eqs = let entry = eqs ! s
                        ss = concatMap exVars (fst entry)
                        eqs' = foldr (adjust (\(es, c) -> (es, c - 1))) (delete s eqs) ss -- O(log(n) * k), k <= maxArrity if flattened
                        in (entry, eqs')

topEquations :: LMMSEquations a -> LMMSEquations a
topEquations = Map.filter (\x -> snd x == 0)

popTopEquations :: Ord a => LMMSEquations a -> (LMMSEquations a, LMMSEquations a)
popTopEquations mmsEqs = let (eqs1, eqs2) = Map.partition (\x -> snd x == 0) mmsEqs
                             ss = concatMap (concatMap exVars . fst) (elems eqs1)
                             eqs2' = foldr (adjust (\(es, c) -> (es, c - 1))) eqs2 ss
                             in (eqs1, eqs2')
