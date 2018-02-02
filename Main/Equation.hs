module Equation where
import           Constraint
import           DAG
import           Data.List
import           Expression
import           UnificationContext
import           Utilities.CustomUtilities

data EqType = LambdaEq | FEq String | AtEq | MMS | ExEq | ClashEq | SymbolicEq deriving(Ord, Show, Eq)
type Equation pi a = (Expression pi a, [Expression pi a], EqType)
eqType (_, _, t) = t
isValidEq (_, _, ClashEq)    = False
isValidEq (_, _, SymbolicEq) = False
isValidEq (_, _, _)          = True

uEq eq = case eqType eq of
              LambdaEq -> True
              FEq _    -> True
              AtEq     -> True
              ExEq     -> True
              _        -> False

mmsEq eq = eqType eq == MMS

type LEquation a = Equation Label a

type Equations pi a = [Equation pi a]
type LEquations a = [LEquation a]

-- functions to build equations
firstGuess e1 = case eType e1 of
                        Lambda -> LambdaEq
                        Fun f  -> FEq f
                        ExSusp -> ExEq
                        AtSusp -> AtEq

eq0 e = (e, [], firstGuess e)
eq e1 = addEx (eq0 e1)

fromList :: [Expression pi a] -> [Equation pi a]
fromList (e:es) = foldr (\e eqs -> concatMap (`addEx` e) eqs) [eq0 e] es
fromList _ = []

addEx (e, es, SymbolicEq) e' = [(e, e':es, SymbolicEq)]

addEx (e, es, LambdaEq) e' = case eType e' of
                                  Lambda -> [(e, e':es, LambdaEq)]
                                  ExSusp -> [(e', e:es, MMS)]
                                  _      -> [(e, e':es, ClashEq)]
addEx (e, es, FEq f) e' = case eType e' of
                               Fun g -> if f == g then [(e, e':es, FEq f)] else [(e, e':es, ClashEq)]
                               ExSusp -> [(e', e:es, MMS)]
                               _ -> [(e, e':es, ClashEq)]
addEx eq@(e, es, AtEq) e' = case eType e' of
                               AtSusp -> [(e, e':es, AtEq)]
                               ExSusp -> [(e', e:es, ExEq)]
                               _      -> [(e, e':es, ClashEq)]
addEx eq@(e, es, ExEq) e' = case eType e' of
                               AtSusp -> [(e, e':es, ExEq)]
                               ExSusp -> [(e, e':es, ExEq)]
                               Lambda -> [eq, (e, [e'], MMS)]
                               Fun f  -> [eq, (e, [e'], MMS)]
addEx eq@(e, es, MMS) e' = case eType e' of
                              Lambda -> [(e, e':es, MMS)]
                              Fun f -> [(e, e':es, MMS)]
                              ExSusp -> [eq, (e, [e'], ExEq)]
                              _      -> [(e, e':es, ClashEq)]

addEx (e, es, ClashEq) e' = [(e, e':es, ClashEq)]
