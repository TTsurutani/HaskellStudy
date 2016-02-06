{-
RubyのEnumerableメソッドを、実装してみようトレーニング
-}
module Enumerable where

-- | all 各要素に対してブロックを評価し、すべての結果 が真である場合に true を返す
-- >>> all2 (>0) [5,6,7]
-- True
-- >>> all2 (>0) [5,-1,7]
-- False

all2::(a->Bool)->[a]->Bool
all2 _ [] = True
all2 f (x:xs) = f x && all2 f xs

{-
all? 
any? 
chunk 
chunk_while 
collect 
map 
collect_concat 
flat_map 
count cycle 
detect 
find 
drop 
drop_while 
each_cons 
each_entry 
each_slice 
each_with_index 
each_with_object 
entries 
to_a 
find_all 
select 
find_index 
first 
grep 
grep_v 
group_by 
include? 
member? 
inject 
reduce 
lazy 
max 
max_by 
min 
min_by 
minmax 
minmax_by 
none? 
one? 
partition 
reject 
reverse_each 
slice_after 
slice_before 
slice_when 
sort 
sort_by 
take 
take_while 
to_h 
zip

-} 