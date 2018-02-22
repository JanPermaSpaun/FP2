> {-# LANGUAGE UnicodeSyntax #-}
> {-# LANGUAGE TypeFamilies #-}
>
> module Minimax2
> where
> -- import Unicode
> import Squiggol
> import Data.List

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

--Klopt dit?? Zie onder

> winning  ∷ Tree position → Bool
> winning (Node e []) = False
> winning (Node e t)  = foldl (||) False (map (losing) t)

> losing :: Tree position -> Bool
> losing (Node e []) = True
> losing (Node e t) = foldl (&&) True (map (winning) t)


> prune :: Integer → Tree elem → Tree elem
> prune 1 (Node q _) = Node q []
> prune n (Node q t) = Node q (map (prune (n-1)) t)

> type Value = Int — [−100 . . 100]
> static :: Position → Value



-- *Minimax2> winning (gametree moves (13,10))
--True
--(0.08 secs, 24,280,928 bytes)
--*Minimax2> winning (gametree moves (14,10))
--True
--(0.11 secs, 36,180,600 bytes)
--*Minimax2> winning (gametree moves (16,10))
--True
--(0.42 secs, 132,001,032 bytes)
--*Minimax2> winning (gametree moves (20,10))
--True
--(6.66 secs, 2,108,523,968 bytes)
--Conclusion: it explodes.

