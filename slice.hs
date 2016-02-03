-- | slice_after
-- >>> slice_after even [0,2,4,1,2,4,5,3,1,4,2]
-- [[0], [2], [4], [1, 2], [4], [5, 3, 1, 4], [2]]

chunk :: (a->Bool)->[a]->[a]
chunk _ [] = []
chunk f [x] 
 | f x = [x]
 | otherwise = []
chunk f (x:y:zs)
 | f x = [x]
 | f y = [x,y]
 | otherwise = x:y:(chunk f zs)

chunkRest :: (a->Bool)->[a]->[a]
chunkRest _ [] = []
chunkRest f [x] 
 | f x = []
 | otherwise = [x]
chunkRest f (x:y:zs)
 | f x = (y:zs)
 | f y = zs
 | otherwise = chunkRest f zs

slice_after:: (a->Bool) ->[a] -> [[a]]
--slice_after _ [] = [[]]
slice_after f xs = (chunk f xs) : (slice_after f (chunkRest f xs))
