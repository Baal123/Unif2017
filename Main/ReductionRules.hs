module ReductionRules where
import           Constraint
import           DAG
import           Data.List
import qualified Data.Map                  as Map
import           Data.Maybe
import           Debug.NoTrace
import           Equation
import           Expression
import           Flattening
import           MMSEquation
import           Permutation
import           Substitution
import           UnificationContext
import           UnificationProblem
import           Utilities.CustomUtilities
import           PermutationInstances

-- typed Rules
-- Atom Equations
atRule :: Equation pi a -> Constraints pi a
atRule (AtomSuspension pi1 a1, es, AtEq)
  = map (FreshEq (pi1, a1) . toBaseTuple) es

-- Function Equations
args f n e = case e of
                  Fn g es -> if f == g && length es == n then Just es
                             else Nothing
                  _ -> Nothing

fRule :: Eq a => String -> Int -> [LExpression a] -> [LEquation a]
fRule f n es
  = let ls = map (args f n) es
        in if Nothing `elem` ls then [(head es, tail es, ClashEq)]
           else
             let ls' = map fromJust ls
                 preEqs = foldr (zipWith (:)) (replicate n []) ls'
                 in concatMap fromList preEqs
-- Lambda Equations
lambdaRule :: Eq a => LExpression a-> LExpression a -> LUnifContext (SwappingList a) -> (LExpression a, LEquations a, LUnifContext (SwappingList a))
lambdaRule (Lam (pi1, a1) e1) (Lam (pi2, a2) e2) lContext
  = let (f1, eq1, lContext1) = flattenAtSusp (pi1, a1) lContext
        (f2, eq2, lContext2) = flattenAtSusp (pi1, a2) lContext1
        (swapping, lContext3) = addTerm [(f1, f2)] lContext2
        (e2', lContext') = addPermL swapping e2 lContext3
        in (Lam (identity, a2) e2, eq e1 e2 ++ [eq1, eq2], lContext')

-- ExpressionSuspension Equations
-- newest sub first
expressionRule0 :: (Show a, Eq a) => LExpression a -> LExpression a -> LUnifContext (SwappingList a) -> (LConstraints a, LExSubstitutions a, LUnifContext (SwappingList a))
expressionRule0 e1@(ExpressionSuspension pi1 s1) e2@(ExpressionSuspension pi2 s2) lContext
  | s1 == s2 = ([CompFixFc pi1 pi2 s1], [], lContext)
  | otherwise = let (sub, lContext') = buildSubstitution e1 e2 lContext
                    in ([], [sub], lContext')
expressionRule0 e se@(ExpressionSuspension pi s) lContext
  = let (sub, lContext') = buildSubstitution se e lContext
        in ([], [sub], lContext')

-- newest sub first
expressionRule :: (Eq a, Show a) => LEquation a -> LUnifContext (SwappingList a) -> (LConstraints a, LExSubstitutions a, LUnifContext (SwappingList a))
expressionRule (e, es, ExEq) lContext
  = let (aEs, sEs) = partition isAtSuspension es
        f e2 (nabla1, theta1, lContext) = let (nabla', theta', lContext') = expressionRule0 e2 e lContext -- pi2 s2 =? pi1 s1 -> s2 -> pi2^-1 pi1 s2
                                              in (nabla' ++ nabla1, theta' ++ theta1, lContext') -- newest first
        (nabla1, theta1, lContext1) = foldr f ([], [], lContext) sEs -- Move: pi2 s2 =? pi1 s1 to theta or nabla
        (nabla2, theta2, lContext2) = case aEs of
                                        [] -> ([], [], lContext1)
                                        (a:as) -> let (sub, lContext') = trace ("Build Sub from: " ++ show (e, a)) buildSubstitution e a lContext1
                                                       in trace ("call atRule with: " ++ show (a:as)) (atRule (a, as, AtEq), [sub], lContext')
        in (nabla2 ++ nabla1, theta2 ++ theta1, lContext2)

-- Something is going wrong here!!
buildSubstitution (ExpressionSuspension pi s) e lContext
  = let (pi', lContext') = trace ("Add Inv: " ++ show pi) addInv pi lContext
        (e', lContext'') = addPermL pi' e lContext'
        in trace ("Sub built: " ++ show (s, e')) ((s, e'), lContext'')


-- Not typed rules
uRule :: (Eq a, Show a) => LEquation a -> SimpleUnificationProblem a -> LUnifContext (SwappingList a) -> (SimpleUnificationProblem a, LUnifContext (SwappingList a))
uRule (e, [], _) up lContext = (up, lContext) -- remove singles except ClashEq
uRule eq@(e, es, t) up@(gamma, nabla, theta) lContext
  =  case t of
          AtEq -> (foldr addToNabla up (atRule eq), lContext)
          FEq f -> let (Fn _ args) = e
                       eqs = let res = fRule f (length args) (e:es) in trace ("FRule result: " ++ show res) res
                       gamma' = let res = foldr addToGammaSimple up eqs in trace ("New Gamma: " ++ show res) res
                       in (gamma', lContext)
          LambdaEq -> let f e1 (e2, eqs, lContext) = let (e2t, eqs', lContext') = lambdaRule e1 e2 lContext
                                                         in (e2t, eqs' ++ eqs, lContext')
                          (lastE, eqs, lContext') = foldr f (e, [], lContext) es
                          in (foldr addToGammaSimple up eqs, lContext')
          ExEq -> let (nabla', theta', lContext') = trace ("Call Experssion Rule on: " ++ show eq) expressionRule eq lContext
                      (gamma', nabla'', lContext'') = trace ("Apply subs: " ++ show theta') applySubs theta' gamma nabla lContext'
                      in ((gamma', nabla' ++ nabla'', theta' ++ theta), lContext'')
          _ -> (addToGammaSimple eq up, lContext) -- Ignore other rules.

-- Help
applySub (s, e2) gamma nabla lContext = let (gamma', lContext1) = subWithLabel s e2 gamma lContext
                                            (nabla', lContext2) = subWithLabel s e2 nabla lContext1
                                            in (gamma', nabla', lContext2)

applySubs subs gamma nabla lContext = let sub sToE (gamma, nabla, lContext') = applySub sToE gamma nabla lContext
                                                 in foldr sub (gamma, nabla, lContext) subs -- apply oldest(last) sub first

-- top level
-- uEqs
workOnUEqs up@([], nabla, theta) lContext = trace ("URules end with: " ++ show up) (up, lContext)
workOnUEqs (eq:gamma, nabla, theta) lContext
  | uEq eq = let (up', lContext') = trace ("Work on Eq: " ++ show eq) uRule eq up lContext
                 in trace ("U-Rule result: " ++ show up') workOnUEqs up' lContext'
  | otherwise = let ((gamma', nabla', theta'), lContext') = workOnUEqs up lContext
                    in ((eq:gamma', nabla', theta'), lContext')
  where
    up = (gamma, nabla, theta)

-- mmsEqs
-- Is called after work on UEqs, if only MMSEquations exist
-- First the computed substitutions need to be applied in reverse order
workOnMMsEqs :: (Show a, Ord a, Dag dag, PrePermutation pi) => LMMSEquations a -> LExSubstitutions a -> dag pi
                                                               -> (LExSubstitutions a, LEquations a, LMMSEquations a, dag pi)
workOnMMsEqs mmsEqs theta lContext = let theta' = filter (isSuspension . snd) theta
                                         f (s, to) (strangeEs, (eqs, dag))
                                           = let (strangeEs', (eqs', dag')) = subAndMerge s to eqs dag in (strangeEs' ++ strangeEs, (eqs', dag'))
                                         (strangeEq, (mmsEqs', lContext')) = foldr f ([], (mmsEqs, lContext)) theta' -- aplpy oldest(last) sub first
                                         (topEquations, otherEqs) = popTopEquations mmsEqs'
                                         (subs, eqs) = toSubsAndEqs topEquations
                                         in (subs ++ theta, eqs ++ fromList strangeEq, otherEqs, lContext')
