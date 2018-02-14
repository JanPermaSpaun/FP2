> {-# LANGUAGE UnicodeSyntax #-}
>	
> module Huffman
> where
> import Unicode
> import Satellite
> import Tree
> import Data.List

-------------------------------------------------------------------------------

Warm-up: constructing a frequency table.

> inc :: Integer -> Integer
> inc x			= x + 1

> size :: Tree elem -> Integer
> size (Leaf a)    	= 1
> size (l :^: r)    	= size(l) + size(r)

> build :: [elem] -> Tree elem
> build [a] = Leaf a
> build as = build (take n as) :^: build (drop n as)
>	where n = length as `div` 2

> frequencies :: (Ord char) ⇒ [char] → [With Int char]
> frequencies xs			= makeWith (group (sort xs))

> makeWith  	:: (Ord char) ⇒ [[char]] → [With Int char]
> makeWith []			= []
> makeWith (x:xs)		= [length x :- head x] ++ makeWith xs


-------------------------------------------------------------------------------

Constructing a Huffman tree.

> huffman			:: [With Int char] -> [With Int (Tree char)]
> huffman xs			= formbranch (huffmanpair xs)

> formbranch		:: [With Int (Tree char)] -> [With Int (Tree char)]
> formbranch (x:xs)		
>	| length xs == 0	= [x]
>	| otherwise			= formbranch ( sort ( [(first (x) + first (head xs)) :- satellite(x) :^: satellite(head xs)] ++ tail xs ))

> huffmanpair		:: [With Int char] → [With Int (Tree char)]
> huffmanpair (x:xs)	= sortedhuff (sort (x:xs))

> sortedhuff		:: [With Int char] -> [With Int (Tree char)]
> sortedhuff []			= []
> sortedhuff (x:xs)		= [first x :- Leaf (satellite (x))] ++ sortedhuff xs

> removefreq			:: [With Int (Tree char)] -> (Tree char)
> removefreq ([x :- y])	= y
		
-- Here, in the "showenglish", I apply the algorithm to 
-- relative frequencies of letters in the English language

> showenglish :: IO ()
> showenglish = print ( huffman (huffmanpair [ 1 :- 'z', 2 :- 'q', 3 :- 'x', 4 :- 'j',
> 	5 :- 'k', 6 :- 'v', 7 :- 'b', 8 :- 'p', 9 :- 'y', 10 :- 'g', 11 :- 'f', 12 :- 'w',
> 	13 :- 'm', 14 :- 'u', 15 :- 'c', 16 :- 'l', 17 :- 'd', 18 :- 'r', 19 :- 'h',
> 	20 :- 's', 21 :- 'n', 22 :- 'i', 23 :- 'o', 24 :- 'a', 25 :- 't', 26 :- 'e']))


-------------------------------------------------------------------------------

Encoding ASCII text.

> data Bit = O | I
> 	deriving (Show, Eq, Ord)

< encode 			:: (Eq char) ⇒ Tree char → [char] → [Bit]

-- To use "codes", first remove the frequencies with "removefreq"

> codes 			:: Tree char → [(char, [Bit])]
> codes (Leaf a :^: Leaf b)		= [(a,[O]) , (b,[I])]
> codes (Leaf a :^: ct)			= [(a,[O])] ++ map (add I) (codes (ct))
> codes (ct :^: Leaf b)			= map (add O) (codes ct) ++ [(b,[I])]
> codes (cta :^: ctb)			= map (add O) (codes cta) ++ map (add I) (codes ctb)

> add				:: Bit -> (char,[Bit]) -> (char,[Bit])
> add (x) (a,bs)				= (a, [x] ++ bs)	

-- Here, in "showcodes", I make the tree of "hello world", remove the frequencies,
-- and make the codetable of this tree.

> showcodes :: IO ()
> showcodes = print ( codes (removefreq (huffman (frequencies "hello world"))))

-------------------------------------------------------------------------------

Decoding a Huffman binary.

< decode ∷ Tree char → [Bit] → [char]

-------------------------------------------------------------------------------

Some test data.

> hw, why ∷ String
> hw =
>   "hello world"

code = huffman (makeWith hw)
encode code hw
decode code it
decode code it == hw

> why =
>   "As software becomes more and more complex, it\n\
>   \is  more  and  more important to structure it\n\
>   \well.  Well-structured  software  is  easy to\n\
>   \write,   easy   to   debug,  and  provides  a\n\
>   \collection  of modules that can be re-used to\n\
>   \reduce future programming costs. Conventional\n\
>   \languages place a conceptual limit on the way\n\
>   \problems   can   be  modularised.  Functional\n\ 
>   \languages  push  those  limits  back. In this\n\
>   \paper we show that two features of functional\n\
>   \languages    in    particular,   higher-order\n\
>   \functions and lazy evaluation, can contribute\n\
>   \greatly  to  modularity.  Since modularity is\n\
>   \the key to successful programming, functional\n\
>   \languages  are  vitally important to the real\n\
>   \world."

code = huffman (makeWith why)
encode code why
decode code it
decode code it == why
