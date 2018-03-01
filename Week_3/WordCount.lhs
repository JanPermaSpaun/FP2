ghc --make WordCount.lhs

> {-# LANGUAGE UnicodeSyntax #-}
>
> module Main
> where
> import Unicode ()
> import System.Environment


> countWords :: String -> Int
> countWords [] = 0
> countWords (x:xs)
>   | x == ' '   = 1 + countWords xs
>   | otherwise  = countWords xs

> main :: IO ()
> main = do
>   args <- getArgs
>   case args of
>       [] -> return ()
>       (x:xs) -> do{
>       file <- readFile x;
>       putStrLn(x ++ "  " ++ show(countWords(file) + 1) ++ "  " ++ (show (length file))); -- Does not really count bytes, not a clue how to do that. Counts words :).
>       }
