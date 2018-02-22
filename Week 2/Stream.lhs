> {-# LANGUAGE UnicodeSyntax #-}
>
> module Stream
> where
> import Prelude hiding (head, tail, repeat, map, zip, take, sum)
> import Unicode

> data Stream elem  =  Cons { head ∷ elem, tail ∷ Stream elem }
>   deriving Show
>
> infixr 5 ≺
> (≺)    ∷  elem → Stream elem → Stream elem
> a ≺ s  =   Cons a s

> from ∷ Integer → Stream Integer
> from n = n ≺ from (n + 1)


> repeat  ∷  a → Stream a
> repeat a = Cons a (repeat a)

> map     ∷  (a → b) → (Stream a → Stream b)
> map f (Cons h t) = Cons (f h) (map f t)

> zip     ∷  (a → b → c) → (Stream a → Stream b → Stream c)
> zip f (Cons h1 t1) (Cons h2 t2) = Cons (f h1 h2) (zip f t1 t2)

> instance (Num elem) ⇒ Num (Stream elem) where
>   as + bs     = zip (+) (as) (bs)
>   as - bs     = zip (-) (as) (bs)
>   as * bs     = zip (*) as bs
>   abs as      = map (abs) as
>   signum as   = map (signum) as
>   fromInteger as = Cons (fromInteger as) (repeat (fromInteger as))

> nat, fib ∷ Stream Integer
> nat  =  0 ≺ nat + 1
> fib  =  0 ≺ 1 ≺ fib + tail fib

> take ∷ Integer → Stream elem → [elem]
> take 0 s = []
> take i s = head s : (take (i - 1) (tail s))

> diff :: (Num elem) => Stream elem -> Stream elem
> diff s  =  tail s - s

> sum :: (Num elem) => Stream elem -> Stream elem
> sum a = Cons 0 (sum a + a)

-- Question unclear??? Guess we can't.
