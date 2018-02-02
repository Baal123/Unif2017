module PermutationInstances where
import           Expression
import           Permutation

type SwappingList a = [(AtomVariable a, AtomVariable a)]

instance Eq a => PrePermutation [a] where
  identity = []

instance Eq a => Permutation [a] where
  inverse = reverse
  add = (++)
