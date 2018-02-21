> {-# LANGUAGE UnicodeSyntax #-}
>
> module Chain
> where
> -- import Unicode
> import Satellite

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
> minimumCost g [a]	= 0 :- a
> minimumCost g as 	= minimum [cmp (c1 :- s1) (c2 :- s2) | (c1 :- s1, c2 :- s2) ← [((minimumCost g bs), (minimumCost g cs)) | (bs, cs) ← split as]]
>	where cmp (c1 :- s1) (c2 :- s2) = (c1 + c + c2) :- s where c :- s = g s1 s2

> showMinimumCost = [(minimumCost (×) [(10, 30), (30, 5), (5, 60)]), (minimumCost (×) [ (i, i + 1) | i <- [1 .. 3] ]), (minimumCost (×) [ (i, i + 1) | i <- [1 .. 9] ])]

> (✚✚) :: [a] → [b] → With Cost Integer
> [] ✚✚ [] = 0 :- 0
> [] ✚✚ (b:bs) = 0 :- 1 + l where (_ :- l) = [] ✚✚ bs
> (a:as) ✚✚ bs = 1 + c :- 1 + l where (c :- l) = as ✚✚ bs


> (✚) :: Integer → Integer → With Cost Integer
> i ✚ j = max (digits i) (digits j) :- digits (i + j)
>	where digits n = if n >= 10 then 1 + (digits (n `div` 10)) else 1



< optimalChain  ∷ (size → size → With Cost size) → [size] → With Cost (With size (Tree size))
