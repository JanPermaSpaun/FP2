> {-# LANGUAGE UnicodeSyntax #-}
>
> module Evaluator
> where
> -- import Unicode

> infixl 6 :+:
> infixl 7 :*:
> infixr 1 :?:
>
> data Expr
>   =  Lit Integer    -- a literal
>   |  Expr :+: Expr  -- addition
>   |  Expr :*: Expr  -- multiplication
>   |  Div Expr Expr  -- integer division
>   |  Expr :?: Expr  -- non-deterministic choice
>   |  Var String     -- a variable

> evalA ∷ (Applicative f) ⇒ Expr → f Integer
> evalA (Lit i)      =  pure i
> evalA (e1 :+: e2)  =  pure (+)  <*> evalA e1 <*> evalA e2
> evalA (e1 :*: e2)  =  pure (*)  <*> evalA e1 <*> evalA e2
> evalA (Div e1 e2)  =  pure div  <*> evalA e1 <*> evalA e2

> toss  ∷  Expr
> toss  =  Lit 0 :?: Lit 1

> z :: Expr
> z = Lit 0

> evalN ∷ Expr → [Integer]
> evalN (Lit i) 	= pure i
> evalN (e1 :+: e2)	= [x + y | x <- evalN e1, y <- evalN e2]
> evalN (e1 :*: e2) = [x * y | x <- evalN e1, y <- evalN e2]
> evalN (Div e1 e2)	= [x `div` y | x <- evalN e1, y <- evalN e2]
> evalN (e1:?: e2)	= evalN e1 ++ evalN e2 

> showEvalN =
>	[evalN toss] ++
>	[evalN (toss :+: Lit 2 :*: toss)] ++
>	[evalN (toss :+: Lit 2 :*: (toss :+: Lit 2 :*: (toss :+: Lit 2 :*: toss)))]

> evalR ∷ Expr → [(String, Integer)] → Integer
> evalR (Lit i)	_ 		= i
> evalR (Var s)	vs		= ret (lookup s vs) where
>	ret (Just i)		= i
>	ret Nothing			= 0
> evalR (e1 :+: e2) vs 	= evalR e1 vs + evalR e2 vs
> evalR (e1 :*: e2) vs 	= evalR e1 vs * evalR e2 vs
> evalR (Div e1 e2) vs 	= evalR e1 vs `div` evalR e2 vs

> showEvalR =
>	[evalR (Var "a" :+: Lit 1) [("a", 4711), ("b", 0815)]] ++ 
>	[evalR (Var "a" :*: Var "b") [("a", 4711), ("b", 0815)]] ++
>	[evalR (Var "a" :*: Var "c") [("a", 4711), ("b", 0815)]]
