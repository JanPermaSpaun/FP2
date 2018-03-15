> {-# LANGUAGE UnicodeSyntax #-}
> {-# LANGUAGE TypeOperators #-}
> {-# LANGUAGE TypeFamilies #-}
> {-# LANGUAGE TypeFamilyDependencies #-}

> module DigitalSearching
> where
> import Prelude hiding (lookup)
> -- import Unicode

> data family Map key ∷ * → *
>
> class (Ord key) ⇒ Key key where
>   empty   ∷  Map key val
>   insert  ∷  key → (Maybe val → val) → Map key val → Map key val
>   lookup  ∷  key → Map key val → Maybe val

> data instance Map ()   val  =  Empty | Single val
>
> instance Key () where
>   empty  =  Empty
>   insert () f (Empty)     =  Single (f Nothing)
>   insert () f (Single v)  =  Single (f (Just v))
>   lookup () (Empty)       =  Nothing
>   lookup () (Single v)    =  Just v

> data instance Map (Either key1 key2) val  = Nada | L key1 val | R key2 val
>
> instance (Key key1, Key key2) ⇒ Key (Either key1 key2) where
>   empty                               = Nada
>   insert (Left l) f (Nada)            = Nada
>   insert (Right r) f (Nada)           = Nada
>   insert (Left l) f (L k v)           = L l (f (Just v))
>   insert (Left l) f (R k v)           = L l (f (Just v))
>   insert (Right r) f (L k v)          = R r (f (Just v))
>   insert (Right r) f (R k v)          = R r (f (Just v))
>   lookup (Left l) Nada                = Nothing
>   lookup (Right r) Nada               = Nothing
>   lookup (Left l) (L key1 val)
>       |l == key1                      = Just val
>       |otherwise                      = Nothing
>   lookup (Right r) (R key1 val)
>       |r == key1                      = Just val
>       |otherwise                      = Nothing

> data instance Map (key1, key2) val  =  Noppes | T (key1, key2) val
>
> instance (Key key1, Key key2) ⇒ Key (key1, key2) where
>   empty                           = Noppes
>   insert (a1,b1) f (Noppes)       = T (a1, b1) (f Nothing)
>   insert t1 f (T t2 v)            = T t2 (f (Just v))
>   lookup t1 (Noppes)              = Nothing
>   lookup t1 (T t2 v)
>       |t1 == t2                   = Just v
>       |otherwise                  = Nothing


> type List elem  =  Either () (elem, [elem])
>
> toList ∷ [elem] → List elem
> toList []        =  Left ()
> toList (a : as)  =  Right (a, as)

> data instance Map [key] val  = Leeg | V [key] val
>
> instance (Key key) ⇒ Key [key] where
>   empty = Leeg
>   insert ls f Leeg                = Leeg
>   insert ls f (V ks val)          = V ls (f (Just val))
>   lookup ls (Leeg)                = Nothing
>   lookup ls (V ks val)            
>       | ls == ks                  = Just val
>       | otherwise                 = Nothing
