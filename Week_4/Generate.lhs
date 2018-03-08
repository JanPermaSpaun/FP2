> {-# LANGUAGE UnicodeSyntax #-}
>
> module Generate
> where
> -- import Unicode

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
> integers =  (pure 2) ++ (pure 3) ++ (pure 4) ++ (pure 5) ++ (pure 6) ++ (pure 7) ++ (pure 8) ++ (pure 9) ++ (pure 10)

> cards :: [Card]
> cards = (pure Joker) ++ (pure Card <*> ranks <*> suits)
 
> data Tree elem = Empty | Node (Tree elem) elem (Tree elem)

> lists :: [elem] -> Integer -> [[elem]]
> lists [] i = []
> lists _ 0 = []
> lists (e:es) i 
>   | i == 1 = [[e]] ++ (combine es i)
>   | i > 1  = [((e:es) !! a) : b | a <- [0 .. (length(e:es) -1)], b <-  (combine (e:es) (i-1))]



--map (x:) (combine (x:xs) (i-1)) ++ (combine xs i)
-- bools := [[True],[False]]

lists bools 1
lists bools 2
trees (lists bools 2) 1
trees (lists bools 2) 2
