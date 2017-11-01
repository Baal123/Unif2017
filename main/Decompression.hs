module Decompression(decomPermDag) where
import           DAG
import           Data.Map    (Map, member, (!))
import qualified Data.Map    as Map
import           Permutation
import           Expression
import           PermutationInstances

decomPermDag :: Permutation pi => PermDAG pi -> Map Label pi
decomPermDag (DAG ls _)
  = let lsM = Map.fromList (map (\x -> (labelKey x, x)) ls)
        in foldl (\dag x -> fst $ decom x dag lsM) Map.empty ls

decom :: Permutation pi => Node pi -> Map Label pi -> Map Label (Node pi) -> (Map Label pi, pi)
decom (LabelTerm l pi) dagDecom dagComp
  = (Map.insert l pi dagDecom, pi)

decom (Inverse l1 l2) dagDecom dagComp
  = let (dagDecom', pi) = lookupOrBuild l2 dagDecom dagComp
        pi' = inverse pi
        in (Map.insert l1 pi' dagDecom', pi')

decom (Concat l1 l2 l3) dagDecom dagComp
  = let (dagDecom2, pi2) = lookupOrBuild l2 dagDecom dagComp
        (dagDecom3, pi3) = lookupOrBuild l3 dagDecom2 dagComp
        pi1 = add pi2 pi3
        in (Map.insert l1 pi1 dagDecom3, pi1)

lookupOrBuild l dagDecom dagComp
  | l `member` dagDecom = (dagDecom, dagDecom ! l)
  | otherwise = decom (dagComp ! l) dagDecom dagComp

-- -- Tests
-- atVar = AtVar . Name
-- a = atVar "a"
-- b = atVar "b"
-- c = atVar "c"
-- d = atVar "d"
-- e = atVar "e"
-- dag0 = DAG [LabelTerm 0 [], LabelTerm 1 [(a, b)], LabelTerm 2 [(b, c)], LabelTerm 3 [(d, e)], LabelTerm 4 [(c, d)]] 5
-- retrievePerm dag = decomPermDag (snd dag) ! fst dag
--
-- -- Concat Everything
-- perm1 = [(a, b)] `add` [(b, c)] `add` [(d, e)] `add` [(c, d)]
-- dag1 = let (n1, d1) = addComp 1 2 dag0
--            (n2, d2) = addComp n1 3 d1
--            in addComp n2 4 d2
--
-- -- test1 = retrievePerm dag1 @?= perm1
-- -- Concat Everthing twice in two versions
-- perm2 = perm1 `add` perm1
-- dag2_1 = let (n0, d0) = dag1
--              (n1, d1) = addComp 1 2 d0
--              (n2, d2) = addComp n1 3 d1
--              (n3, d3) = addComp n2 4 d2
--              in addComp n0 n3 d3
--
-- dag2_2 = let (n0, d0) = dag1
--              (n1, d2) = addComp 0 n0 d0
--              in addComp n0 n1 d2
--
-- -- test2_1 = retrievePerm dag2_1 @?= perm2
-- -- test2_2 = retrievePerm dag2_2 @?= perm2
--
-- -- Reverse big concat
-- perm3 = inverse perm2
-- dag3 = uncurry addInv dag2_2
-- -- test3 = retrievePerm dag3 @?= perm3
-- --
-- -- decomTests = testGroup "FlatteningTestGroup"
-- --                   [testCase "TestConcat1" test1,
-- --                    testCase "TestConcat2_1" test2_1,
-- --                    testCase "TestConcat2_2" test2_2,
-- --                    testCase "TestReverse" test3]
