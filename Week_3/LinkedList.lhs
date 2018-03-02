> {-# LANGUAGE UnicodeSyntax #-}
>
> module LinkedList
> where
> -- import Unicode
> import Data.IORef

> type ListRef elem  =  IORef (List elem)
>
> data List elem  =  Nil | Cons elem (ListRef elem)

-------------------------------------------------------------------------------

Bas Steeg - s4259181
Rick Lukassen - s4263812
David van Oorsouw - s4076605

-------------------------------------------------------------------------------

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
>					Cons x next -> do 	{xs <- toList next
>										; return (x:xs)} 

> foreach :: ListRef a → (a → IO b) → IO (ListRef b)
> foreach ref f = do
>				list <- readIORef ref
>				case list of
>					Nil			-> newIORef Nil
>					Cons x next -> do 	{e <- f x
>										; xs <- foreach next f
>										; cons e xs} 
