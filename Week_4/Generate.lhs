> {-# LANGUAGE UnicodeSyntax #-}
>
> module Generate
> where
> -- import Unicode

-------------------------------------------------------------------------------

Bas Steeg - s4259181
Rick Lukassen - s4263812
David van Oorsouw - s4076605

-------------------------------------------------------------------------------l

> bools  ∷  [Bool]
> bools  =  pure False ++ pure True
>
> maybes  ∷  [elem] → [Maybe elem]
> maybes elems  =  pure Nothing ++ (pure Just <*> elems)

> data Suit  =  Spades | Hearts | Diamonds | Clubs
> data Rank  =  Faceless Integer | Jack | Queen | King
> data Card  =  Card Rank Suit | Joker

> suits :: [Suit]
> suits = (pure Spades) ++ (pure Hearts) ++ (pure Diamonds) ++ (pure Clubs)

> ranks :: [Rank]
> ranks = (pure Faceless <*> integers) ++ (pure Jack) ++ (pure Queen) ++ (pure King)

> integers :: [Integer]
> integers =  [a | b <- [2..10], a <- (pure b)]

> cards :: [Card]
> cards = (pure Joker) ++ (pure Card <*> ranks <*> suits)
 
> data Tree elem = Empty | Node (Tree elem) elem (Tree elem)

> lists :: [elem] -> Integer -> [[elem]]
> lists [] i =  []
> lists _ 0 = []
> lists (e:es) i 
>   | i == 1 = [[e]] ++  (lists es i)
>   | i > 1  = [((e:es) !! a) : b | a <- [0 .. (length(e:es) -1)], b <-  (lists (e:es) (i-1))]



--map (x:) (combine (x:xs) (i-1)) ++ (combine xs i)
-- bools := [[True],[False]]

lists bools 1
lists bools 2
trees (lists bools 2) 1
trees (lists bools 2) 2
