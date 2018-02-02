module Permutation where
class Eq e => PrePermutation e where
  identity :: e

class PrePermutation e => Permutation e where
  inverse :: e -> e
  add :: e -> e -> e         -- add pi1 pi2 == pi1 comp pi2
