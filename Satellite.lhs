> {-# LANGUAGE UnicodeSyntax #-}
>
> module Satellite
> where

Satellite data.

> infix 1 :-
> data With a b  =  a :- b
>   deriving (Show)
>
> satellite ∷ With a b → b
> satellite (_ :- b)  =  b
>
> earth ∷ With a b → a
> earth (a :- _)  =  a

> instance (Eq a) ⇒ Eq (With a b) where
>   (a :- _) == (b :- _)  =  a == b
> instance (Ord a) ⇒ Ord (With a b) where
>   (a :- _) <= (b :- _)  =  a <= b
