> {-# LANGUAGE UnicodeSyntax #-}
>
> module RandomAccessList
> where
> -- import Unicode

> data Nat  =  Z | S Nat deriving Show

> data List elem  =  Zero | Succ elem (List elem) deriving Show

> data Bin  =  N | O Bin | I Bin deriving Show

> type Pair elem = (elem, elem) 

> data Sequ elem
>   =  Nil 
>   |  OCons       (Sequ (Pair elem))
>   |  ICons elem  (Sequ (Pair elem)) deriving Show

> unary   ∷ Bin  → Nat
> unary (N)			= Z
> unary (O bin)		= twice (unary bin)
> unary (I bin)		= S (twice (unary bin))

> twice :: Nat -> Nat 
> twice	(Z)				= Z
> twice (S nat)		= S (S (twice nat))

> binary  ∷ Nat  → Bin
> binary (Z)			= N
> binary (S nat)		= succ' (binary nat)

> succ'	:: Bin → Bin
> succ' (N)		= I (N)
> succ' (O bin)  = I (bin)
> succ' (I bin) 	= O (succ' (bin))

> toList    ::  Sequ elem  → List elem
> toList	(Nil)											= Zero 
> toList (OCons n)									= flatten (toList n)
> toList (ICons a n)								= Succ a (flatten (toList n))

> flatten :: (List (Pair a)) → List a
> flatten Zero 					= Zero
> flatten (Succ (e1,e2) es)	= Succ e1  (Succ e2  (flatten es))

> fromList  ∷  List elem  → Sequ elem
> fromList Zero					= Nil

> cons' :: elem → Sequ elem → Sequ elem
> cons' e Nil						= ICons e (Nil)
> cons' e (OCons n)			= ICons e n
> cons' e1 (ICons e2 n) 		= OCons n -- Dit klopt niet, moet de recursie in. Ik dacht aan OCons (cons' e1 (cons' e2 n)), maar dat compileert niet!