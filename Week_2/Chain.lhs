> {-# LANGUAGE UnicodeSyntax #-}
>
> module Chain
> where
> import Unicode
> import Satellite
> import Tree
> import Data.Array

Costs and dimensions.

> type Cost  =  Integer
> type Dim   =  (Integer, Integer)

> (×) ∷ Dim → Dim → With Cost Dim
> (i, j) × (j', k)
>   | j == j'    =  (i * j * k) :- (i, k)
>   | otherwise  =  error "(×): dimensions do not match"

> (<×>) ∷ With Cost Dim → With Cost Dim → With Cost Dim
> (c1 :- d1) <×> (c2 :- d2)
>   =  (c1 + c + c2) :- d where c :- d =  d1 × d2

Minimal costs.

> minCost ∷ [Dim] → With Cost Dim
> minCost [a]  =  0 :- a
> minCost as   =  minimum [ minCost bs <×> minCost cs | (bs, cs) ← split as ]

> split ∷ [a] → [([a], [a])]
> split []        =  error "split: empty list"
> split [_a]      =  []
> split (a : as)  =  ([a], as) : [ (a : bs, cs) | (bs, cs) ← split as]

minCost [(10, 30), (30, 5), (5, 60)]
minCost [ (i, i + 1) | i <- [1 .. 3] ]
minCost [ (i, i + 1) | i <- [1 .. 9] ]

> minimumCost   ∷ (size → size → With Cost size) → [size] → With Cost size
> minimumCost f [a]	= 0 :- a
> minimumCost f as 	= minimum [cmp (c1 :- s1) (c2 :- s2) | (c1 :- s1, c2 :- s2) ← [((minimumCost f bs), (minimumCost f cs)) | (bs, cs) ← split as]]
>	where cmp (c1 :- s1) (c2 :- s2) = (c1 + c + c2) :- s where c :- s = f s1 s2

> showMinimumCost = [(minimumCost (×) [(10, 30), (30, 5), (5, 60)]), (minimumCost (×) [ (i, i + 1) | i <- [1 .. 3] ]), (minimumCost (×) [ (i, i + 1) | i <- [1 .. 9] ])]

> (✚✚) ∷ Integer → Integer → With Cost Integer
> a ✚✚ b = a :- (a + b)

> (✚) ∷ Integer → Integer → With Cost Integer
> i ✚ j = max i j :- i + j

Naive optimal chain

> crappyOptimalChain ∷ (size → size → With Cost size) → [size] → With Cost (With size (Tree size))
> crappyOptimalChain f [a] = 0 :- (a :- (Leaf a))
> crappyOptimalChain f as = minimum [cmp (c1 :- s1) (c2 :- s2) | (c1 :- s1, c2 :- s2) ← [((crappyOptimalChain f bs), (crappyOptimalChain f cs)) | (bs, cs) ← split as]]
> 	where cmp (c1 :- (s1 :- t1)) (c2 :- (s2 :- t2)) = (c1 + c + c2) :- (s :- t1 :^: t2) where c :- s = f s1 s2

Split a list into all possible ways to partition the non-empty argument list into two non-empty lists, represented by tuples of the first and last element of a list.

> splitSeg ∷ Int → Int → [((Int, Int), (Int, Int))]
> splitSeg a b = [((a, i), (i + 1, b)) | i ← [a..(b - 1)]]

DP optimal chain

> optimalChain ∷ (size → size → With Cost size) → [size] → With Cost (With size (Tree size))
> optimalChain f as  = memof ! (0, n) where
>	memof = array ((0,0), (n,n)) [((i, j), optimalChain' f (i, j)) | i ← [0..n], j ← [i..n]]
>	n 	  = (length as) - 1
> 	optimalChain' f (i, j)
>		| i == j  = 0 :- ((as !! i) :- (Leaf (as !! i))) 
>		| otherwise = minimum [cmp (c1 :- s1) (c2 :- s2) | (c1 :- s1, c2 :- s2) ← [((memof ! bs), (memof ! cs)) | (bs, cs) ← splitSeg i j]]
> 	cmp (c1 :- (s1 :- t1)) (c2 :- (s2 :- t2)) = (c1 + c + c2) :- (s :- t1 :^: t2) where c :- s = f s1 s2
>	slice (from, to) = take (to - from + 1) (drop from as)


> showListConcatTree = optimalChain (✚✚) [50, 10, 99]
> showIntegerAdditionTree = optimalChain (✚) [50, 10, 99]

2.2.4:

List concatenation is right-associative because this is the cheapest way of concatenating lists.
The optimal chain shows that right-associative concatenation of [50, 10, 99] results in a list of 159 elements with cost 60:

 showListConcatTree = optimalChain (✚✚) [50, 10, 99]
 60 :- (159 :- Leaf 50 :^: (Leaf 10 :^: Leaf 99))

While the cost of first concatenating a list of 50 elements with one of 90 elements:

 showListConcatTree = optimalChain (✚✚) [50, 10]
 50 :- (60 :- Leaf 50 :^: Leaf 10)
 
Followed by concatenating the resulting list of 60 elements to one of 99:

 showListConcatTree = optimalChain (✚✚) [60, 99]
 60 :- (159 :- Leaf 60 :^: Leaf 99)
 
Results in a total cost of 50 + 60.