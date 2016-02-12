module Chunk where
-- chunkWhile	
-- | 1ずつ増加する部分配列ごとに分ける。
-- >>> chunkWhile oneplus [1,2,4,9,10,11,12,15,16,19,20,21]
-- [[1,2],[4],[9,10,11,12],[15,16],[19,20,21]]

-- |
-- >>> chunkWhile oneplus [1,2,4,9,10,11,12,15,16,19,20,21,23]
-- [[1,2],[4],[9,10,11,12],[15,16],[19,20,21],[23]]

-- | 増加のみの部分配列ごとに分ける。
-- >>> chunkWhile increase [0,9,2,2,3,2,7,5,9,5]
-- [[0,9],[2,2,3],[2,7],[5,9],[5]]

chunkWhile :: (Ord a,Eq a) => (a->a->Bool)->[a]->[[a]]
chunkWhile _ [] = []
chunkWhile _ [x] = [[x]]
chunkWhile p xs = chunk p xs : chunkWhile p (dropList (chunk p xs) xs)

-- | 条件が偽になるところでチャンクを作る関数
-- >>> chunk oneplus [1,2,4,9,10,11,12,15,16,19,20,21]
-- [1,2]
-- >>> chunk oneplus [9,10,11,12,15,16,19,20,21]
-- [9,10,11,12]
-- >>> chunk oneplus [20,21,23]
-- [20,21]
-- >>> chunk oneplus [21,22]
-- [21,22]
-- >>> chunk oneplus [21,23]
-- [21]
-- >>> chunk oneplus [23]
-- [23]
chunk :: (a->a->Bool) -> [a] -> [a]
chunk _ [] = []
chunk p [x] = [x]
chunk p [x,y]
 | p x y = [x,y]
 | otherwise = [x]
chunk p (x:y:zs)
 | p x y = x:chunk p (y:zs)
 | otherwise = [x]

-- >>> dropList [1,2] [1,2,4,9,10,11,12,15,16,19,20,21]
-- [4,9,10,11,12,15,16,19,20,21]

dropList::Eq a =>[a]->[a]->[a]
dropList _ [] = []
dropList [] xs = xs
dropList (x:xs) (y:ys)
 |x == y = dropList xs ys
 |otherwise = y:ys

-- | １ずつ増加したら真になる関数
-- >>> oneplus 1 2
-- True
-- >>> oneplus 2 4
-- False
oneplus :: (Num a,Eq a) => a -> a -> Bool
--oneplus :: Int-> Int -> Bool
oneplus x y
 | x + 1 == y = True
 | otherwise = False

-- |
-- >>> increase 1 2
-- True
-- >>> increase 2 1
-- False

increase :: (Ord a,Eq a) => a -> a -> Bool
increase x y 
 | x <= y = True
 | otherwise = False

{--
-- | 条件にマッチするリストを簡約する
-- >>> formatFromTo [9,10,11,12]
-- "9-12"
-- >>> formatFromTo [1,2]
-- ["1","2"]
-- | つなぐ
-- >>> joit2Char ls
-- "1,2,4,9-12,15,16,19-21"



隣り合う偶数同士、奇数同士の部分配列ごとに分ける。
(Enumerable#chunk を使って実現する事も可能)
a = [7,5,9,2,0,7,9,4,2,0]
p a.chunk_while {|i, j| i.even? == j.even? }.to_a
# => [[7,5,9],[2,0],[7,9],[4,2,0]]
--}