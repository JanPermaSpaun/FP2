ghc --make WordCount.lhs

> {-# LANGUAGE UnicodeSyntax #-}
>
> module Main
> where
> import Unicode ()
> import System.Environment

-------------------------------------------------------------------------------

Bas Steeg - s4259181
Rick Lukassen - s4263812
David van Oorsouw - s4076605

-------------------------------------------------------------------------------

> main :: IO ()
> main = do {
>		args <- getArgs;
>		putCounts args (0,0,0);
>	} where 
>	putCounts [] (lc, wc, cc) = do {
>		putStrLn(show(lc) ++ " " ++ show(wc) ++ " " ++ show(cc) ++ " total");
>	}
>	putCounts (x:xs) (lc, wc, cc) = do {
>		f <- readFile x;
>		putStrLn(show(length $ lines f) ++ " " ++ show(length $ words f) ++ " " ++ show(length f) ++ " " ++ x);		
>		putCounts xs (lc + (length $ lines f), wc + (length $ words f), cc + (length f));
>	}
