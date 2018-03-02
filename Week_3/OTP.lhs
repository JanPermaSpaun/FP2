ghc --make OTP.lhs


> {-# LANGUAGE UnicodeSyntax #-}
>
> module Main
> where
> import Unicode ()
> import System.Random
> import System.Environment
-------------------------------------------------------------------------------

Bas Steeg - s4259181
Rick Lukassen - s4263812
David van Oorsouw - s4076605

-------------------------------------------------------------------------------

> dice :: IO Int
> dice = getStdRandom (randomR (1,6))
>
> roll ∷ IO Int
> roll  =  do  a ← dice
>              b ← dice
>              return (a + b)


-- Seq of random #s and string to be decoded

> encode :: Int -> Int -> Int
> encode a b
>   | a < 32 = a
>   | otherwise = ((a - 32 + b) `mod` (128 - 32)) + 32

> decode :: Int -> Int -> Int
> decode a b
>   | a < 32 = a
>   | otherwise = ((a - 32 - b) `mod` (128 - 32)) + 32

> encodes :: [Int] -> String -> String
> encodes rs as = map toEnum (zipWith encode (map fromEnum as) rs)

> decodes :: [Int] -> String -> String
> decodes rs as = map toEnum (zipWith decode (map fromEnum as) rs)


> main :: IO ()
> main = do
>   setStdGen (mkStdGen 4711)
>   args <- getArgs
>   case args of
>       ["encrypt", a, b] -> do{
>       file <- readFile a;
>       randoms <- sequence (replicate (length file)(roll));
>       writeFile b (encodes (randoms) file)
>       }
>       ["decrypt", a, b] -> do{
>       file <- readFile a;
>       randoms <- sequence (replicate (length file)(roll));
>       writeFile b (decodes (randoms) file)
>       }
>       otherwise -> return ()

-- sequence (replicate (length file)(roll))
-- return :: a -> IO a
-- (>=) :: IO a -> (a -> IO b) -> IO b
-- sequence :: Monad m => [m a] -> m [a]
