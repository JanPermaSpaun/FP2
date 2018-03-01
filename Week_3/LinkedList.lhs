> {-# LANGUAGE UnicodeSyntax #-}
>
> module LinkedList
> where
> -- import Unicode
> import Data.IORef

> type ListRef elem  =  IORef (List elem)
>
> data List elem  =  Nil | Cons elem (ListRef elem)

> nil  :: IO (ListRef elem)
> nil	= newIORef Nil

> cons :: elem → ListRef elem → IO (ListRef elem)
> cons e lr = newIORef (Cons e lr) 

> fromList :: [elem] → IO (ListRef elem)
> fromList []		= nil
> fromList (x:xs)	= do { ys <- (fromList xs)
>					     ; cons x ys}			 

> toList   :: ListRef elem → IO [elem]
> toList ref	= do 
>				list <- readIORef ref
>				case list of
>					Nil			-> return []
>					Cons x next -> do {xs <- toList next; return (x:xs)} 

< foreach ∷ ListRef a → (a → IO b) → IO (ListRef b)
