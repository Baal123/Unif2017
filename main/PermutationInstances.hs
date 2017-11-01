module PermutationInstances where
import           Expression
import           Permutation

type SwappingList a = [(AtomVariable a, AtomVariable a)]

instance PrePermutation [a] where
  identity = []

instance Permutation [a] where
  inverse = reverse
  add = (++)
