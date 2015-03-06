module Utils where

import Control.Arrow

update :: (a -> a) -> Int -> [a] -> [a]
update _ _ [] = error "update: bigger than length"
update _ n _ | n < 0 = error "update: smaller than 0"
update f 0 (h:t) = (f h):t
update f n (h:t) = h:update f (n - 1) t

partitionWhile :: (a -> Bool) -> [a] -> ([a], [a])
partitionWhile _ [] = ([], [])
partitionWhile f l@(h:t)
  | f h = first (h:) $ partitionWhile f t
  | otherwise = ([], l)
