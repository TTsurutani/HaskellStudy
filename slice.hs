module Foo where

-- | sliceAfter
-- >>> sliceAfter even [0,2,4,1,2,4,5,3,1,4,2]
-- [[0],[2],[4],[1,2],[4],[5,3,1,4],[2]]
-- >>> sliceAfter odd [0,2,4,1,2,4,5,3,1,4,2]
-- [[0,2,4,1],[2,4,5],[3],[1],[4,2]]

chunk :: (a->Bool)->[a]->[a]
chunk _ [] = []
chunk f (x:xs)
 | f x = [x]
 | otherwise = x: chunk f xs


chunkRest :: (a->Bool)->[a]->[a]
chunkRest _ [] = []
chunkRest f (x:xs)
 | f x = xs
 | otherwise = chunkRest f xs

sliceAfter:: (a->Bool) ->[a] -> [[a]]
sliceAfter _ [] = []
sliceAfter f xs = chunk f xs : sliceAfter f (chunkRest f xs)