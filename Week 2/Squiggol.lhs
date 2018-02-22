> {-# LANGUAGE UnicodeSyntax #-}
> {-# LANGUAGE TypeOperators #-}
> {-# LANGUAGE TypeFamilies #-}
>
> module Squiggol
> where
> import Unicode

Base functor.

> class (Functor f) ⇒ Base f where
>   type Rec f ∷ *
>
>   inn  ∷  f (Rec f) → Rec f  -- tying the recursive knot
>   out  ∷  Rec f → f (Rec f)  -- untying the recursive knot

Fold and unfold.

> fold ∷ (Base f) ⇒ (f a → a) → (Rec f → a)
> fold alg = consume
>   where consume = alg ∘ fmap consume ∘ out 

> unfold ∷ (Base f) ⇒ (a → f a) → (a → Rec f)
> unfold coalg = produce
>   where produce = inn ∘ fmap produce ∘ coalg

Para- and apomorphism.

> data a × b  =  a :@ b
>
> (▵) ∷ (x → a) → (x → b) → (x → a × b)
> (f ▵ g) x  =  f x :@ g x

> para ∷ (Base f) ⇒ (f (Rec f × a) → a) → (Rec f → a)
> para alg = consume
>   where consume = alg ∘ fmap (id ▵ consume) ∘ out

> data a + b  =  Stop a | Go b
>
> (▿) ∷ (a → x) → (b → x) → (a + b → x)
> (f  ▿ _g) (Stop  a)  =  f a
> (_f ▿ g ) (Go    b)  =  g b

> apo  ∷ (Base f) ⇒ (a → f (Rec f + a)) → (a → Rec f)
> apo coalg = produce
>   where produce = inn ∘ fmap (id ▿ produce) ∘ coalg

Base functor for Haskell's list datatype.

> data List elem list  =  Nil | Cons elem list

> instance Functor (List elem) where
>   fmap _f  (Nil)       = Nil
>   fmap f   (Cons x xs)  = Cons x (f xs)

> instance Base (List elem) where
>   type Rec (List elem) = [elem]
>
>   inn (Nil)        =  []
>   inn (Cons x xs)  =  x : xs
>
>   out []           =  Nil
>   out (x : xs)     =  Cons x xs
