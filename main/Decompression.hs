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
