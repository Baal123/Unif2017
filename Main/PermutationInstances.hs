module PermutationInstances where
import           Expression
import           Permutation

type Swapping a = (AtomVariable a, AtomVariable a)
type SwappingList a = [Swapping a]

instance Eq a => PrePermutation [a] where
  identity = []

instance Eq a => Permutation [a] where
  inverse = reverse
  add = (++)
