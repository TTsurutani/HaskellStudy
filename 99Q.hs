-- | Problem 1
-- >>> myLast [1,2,3,4]
-- 4
-- >>> myLast ['x','y','z']
--'z'

module Foo where
myLast::[a]->a
myLast = head.reverse
myLast' [] = error "myLastError"
myLast' [x] = x
myLast' (x:xs) = myLast xs

-- !!,fold

-- | q2
-- >>> myButLast [1,2,3,4]
-- 3
-- >>> myButLast ['a'..'z']
-- 'y'

myButLast::[a]->a
myButLast [] = error "no elements"
myButLast [x] = error "only one element"
myButLast [x,_] = x
myButLast (_:xs) = myButLast xs

-- | q3
-- >>> elementAt [1,2,3] 2
-- 2
-- >>> elementAt "haskell" 5
-- 'e'
elementAt::[a]->Int->a
elementAt xs n = xs !! (n-1)
elementAt' [] _ = error "out of range"
elementAt' (x:_) 1 = x 
elementAt' (x:xs) n = elementAt' xs (n-1)

-- | q4
-- >>> myLength [123, 456, 789]
-- 3
-- >>> myLength "Hello, world!"
-- 13
myLength::[a]->Int
-- myLength = foldr (\n ) 0
myLength [] = 0
myLength (_:xs) = 1 + myLength xs

-- | q5
-- >>> myReverse "A man, a plan, a canal, panama!"
-- "!amanap ,lanac a ,nalp a ,nam A"
-- >>> myReverse [1,2,3,4]
-- [4,3,2,1]

myReverse::[a]->[a]
myReverse [] = []
myReverse (x:xs) = myReverse xs ++ [x]

-- | q6
-- >>> isPalindrome [1,2,3]
-- False
-- >>> isPalindrome "madamimadam"
-- True
-- >>> isPalindrome [1,2,4,8,16,8,4,2,1]
-- True

isPalindrome::Eq a => [a]->Bool
isPalindrome [] = True
isPalindrome list
 | reverse list == list = True
 | otherwise = False