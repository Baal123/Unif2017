module Utilities.CustomUtilities where
import           Data.Either
import           Data.Map    ((!))
import           Debug.NoTrace
import           Data.List.Extra
-- Failures
--data Failure = FreshFail | SubFail | SolveFail | TooLarge | WrongEquationType | VarFail | Clash | CycleInCompoundEquations | UnexpectedFailure String deriving(Show, Eq, Ord)
data Failure a = Fail a String deriving(Eq, Ord)
instance Show a => Show (Failure a) where
  show (Fail a msg) = msg ++ "Corresponding data:\n"++ show a


-- utility functions
zipWith1 :: (a -> a -> a) -> [a] -> [a] -> [a]
zipWith1 f [] ls2            = ls2
zipWith1 f ls1 []            = ls1
zipWith1 f (l1:ls1) (l2:ls2) = f l1 l2:zipWith1 f ls1 ls2

-- update rules
update f 0 (x:xs) = f x:xs
update f k (x:xs) = x:update f (k - 1) xs

updateOrInsert f k ls deflt
    | k < length ls = update f k ls
    | otherwise = ls ++ [deflt | n <- [(length ls)..(k - 1)]] ++ [f deflt]

mapWithIndex :: (Int -> a -> b) -> [a] -> [b]
mapWithIndex f xs = map2 f xs 0
                    where
                        map2 f [] c     = []
                        map2 f (x:xs) c = f c x:map2 f xs (c + 1)

mapWithContext f xs c = let g x (ys, c) = let (y, c') = f x c in (y:ys, c')
                            (ys, c') = foldr g ([], c) xs
                            in (ys, c')
-------
repeat2 go start
    | next == start = start
    | otherwise = repeat2 go next
    where
        next = go start

repeat3 go start
    | next == start = start
    | otherwise = repeat3 go next
    where
        next = start >>= go
-----------
iterateWithIndex f k [] end = end
iterateWithIndex f k (x:xs) current = iterateWithIndex f (k + 1) xs (f k x current)

foldM :: Monad m => (a -> b -> m b) -> [a] -> b -> m b
foldM f [] end         = return end
foldM f (x:xs) current = f x current >>= foldM f xs
------------

getOrDefault :: Int -> [a] -> a -> a
getOrDefault k ls deflt
    | k < length ls = ls!!k
    | otherwise = deflt

-----------
mapToFunction m k = m ! k

fromRight (Right x) = x

fromLeft (Left x)  = x
fromLeft (Right x) =  traceShow x error "left called but is not left"

joinMonadList :: Monad m => [m a] -> m [a]
joinMonadList []     = return []
joinMonadList (e:es) = e >>= (\x -> (fmap (\ls -> x:ls) (joinMonadList es)))

mergeOn _ _ [] = []
mergeOn p f es  = foldl1 f toBeMerged : rest
                      where
                        (toBeMerged, rest) = decomp p es ([], [])
                        decomp p (e:es) (ls, rs)
                          | p e = (e:ls, rs)
                          | otherwise = (ls, e:rs)
                        decomp p [] x = x

addMaybe (Just x) xs = x:xs
addMaybe Nothing xs  = xs

fromMaybeList []           = []
fromMaybeList (Just x:xs)  = x:fromMaybeList xs
fromMaybeList (Nothing:xs) = fromMaybeList xs

class Functor2 c where
  fmap2 :: (a -> b) -> c a d -> c b d

-- concatMapWith concatWith mapTo
concatMapWith _ _ [] = []
concatMapWith concatWith mapTo ls = foldl1 concatWith . map mapTo $ ls

sortUniq :: Ord a => [a] -> [a]
sortUniq = sort . nubOrd
