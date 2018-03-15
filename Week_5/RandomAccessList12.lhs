> {-# LANGUAGE UnicodeSyntax #-}
>
> module RandomAccessList12
> where
> -- import Unicode
> import Prelude hiding (head, tail)

> type Pair elem = (elem, elem)

> data Sequ elem
>	= Nil
>	| One	   (Sequ (Pair elem))
>	| Two elem (Sequ (Pair elem))
>  deriving Show

> nil   ∷ Sequ elem
> nil = Nil

> cons  ∷ elem → Sequ elem → Sequ elem
> cons a (Nil)			= Two a Nil
> cons a (One s)		= Two a s
> cons a1 (Two a2 s)	= One (cons (a1, a2) s) 

> head  ∷ Sequ elem → elem
> head (Nil) 			= error "Head of empty Sequ"
> head (One s) 			= fst (head s)
> head (Two a s)		= a 


> tail  ∷ Sequ elem → Sequ elem
> tail (Nil)		= error "Tail of empty Sequ"
> tail (One s) 		= Two (snd (head s)) (tail s)
> tail (Two a s)	= (One s)

> (!)   ∷ Sequ elem → Integer → elem
> Nil	  ! n	= error "Index out of bounds"
> One   s ! n	= s!(n `div` 2) !!! (n `mod` 2)
> Two a s ! 0	= a
> Two a s ! n	= s!((n-1) `div` 2) !!! ((n-1) `mod` 2)
> (a,  _) !!! 0	= a
> (_,  b) !!! 1	= b

If a zero would be used in this system, the One would need to carry a single element (to go from an empty Sequ to a single-element Sequ), and the Two would carry two elements.
A number of leading zeros would have to be used to indicate the value of a One or Two by its position. The time to access and element would become logarithmic in the size of the Sequ,
while, as it stands, this is logarithmic in the index.