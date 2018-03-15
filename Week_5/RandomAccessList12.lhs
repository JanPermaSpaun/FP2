> {-# LANGUAGE UnicodeSyntax #-}
>
> module RandomAccessList12
> where
> -- import Unicode

< data Sequ elem
<
< nil   ∷ Sequ elem
< cons  ∷ elem → Sequ elem → Sequ elem
< head  ∷ Sequ elem → elem
< tail  ∷ Sequ elem → Sequ elem
< (!)   ∷ Sequ elem → Integer → elem
