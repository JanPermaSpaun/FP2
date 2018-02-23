> {-# LANGUAGE UnicodeSyntax #-}
> {-# LANGUAGE TypeFamilies #-}
>
> module Minimax2
> where
> -- import Unicode
> import Squiggol
> import Data.List


-------------------------------------------------------------------------------

Bas Steeg - s4259181
Rick Lukassen - s4263812
David van Oorsouw - s4076605

-------------------------------------------------------------------------------

Multiway trees.

> data Tree elem  =  Node elem [Tree elem]
>   deriving Show

> type Position = (Integer, Integer)

> moves :: Position → [Position]
> moves (1,1) = []
> moves (a,b) = remDouble ([(c,d) | c <- [0..a], d <- [0..a], (c + d) == a, c > 0, d > 0] ++ [(e,f) | e <- [0..b], f <- [0..b], (f + e) == b, f > 0, e > 0])

--removes double occurrences ((1,7) is essentially the same as (7,1)).

> remDouble :: [Position] -> [Position]
> remDouble [] = []
> remDouble ((a,b):xs)
>   | (a,b) `elem` xs = remDouble xs
>   | (b,a) `elem` xs = remDouble xs
>   | otherwise = [(a,b)] ++ (remDouble xs)

> size, depth ∷ Tree elem → Integer
> size (Node a []) = 1
> size (Node e t) = 1 + (sizeb t)

> sizeb :: [Tree elem] -> Integer
> sizeb [] = 0
> sizeb (t:ts) = (size t) + (sizeb ts)

> depth (Node a []) = 1
> depth (Node e t) = 1 + maximum (map depth t)

> gametree ∷ (position → [position]) → (position → Tree position)
> gametree mov p = Node p [(gametree mov x) | x <- (mov p)]

> winning  ∷ Tree position → Bool
> winning (Node e []) = False
> winning (Node e t)  = foldl (||) False (map (losing) t)

> losing :: Tree position -> Bool
> losing (Node e []) = True
> losing (Node e t) = foldl (&&) True (map (winning) t)


> prune :: Integer → Tree elem → Tree elem
> prune 1 (Node q _) = Node q []
> prune n (Node q t) = Node q (map (prune (n-1)) t)

> type Value = Int 

> static :: Position -> Value
> static (x,y)
>			|	x `mod` 2 == 0	= 100
>			|	y `mod` 2 == 0  = 100
>			| 	otherwise 		= -100

> maximize :: (position -> Value) -> (Tree position -> Value)
> maximize f (Node e []) = f e
> maximize f (Node e t) = maximum (map (minimize f) t)

> minimize :: (position -> Value) -> (Tree position -> Value)
> minimize f (Node e []) = f e
> minimize f (Node e t) = minimum (map (maximize f) t)




