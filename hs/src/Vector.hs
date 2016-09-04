{-# LANGUAGE GADTs, DataKinds, KindSignatures, TypeFamilies #-}

module Vector where

data Nat = Z | S Nat

data Vec :: * -> Nat -> * where
  Nil :: Vec a Z
  Cons :: a -> Vec a n -> Vec a (S n)

data SNat n where
  SZ :: SNat Z
  SS :: SNat n -> SNat (S n)

rep :: SNat n -> a -> Vec a n
rep SZ a = Nil
rep (SS n) a = Cons a (rep n a)

type family Plus(n :: Nat)(m :: Nat) :: Nat where
  Plus Z m = m
  Plus (S n) m = S (Plus n m)

sPlus :: SNat n -> SNat m -> SNat (Plus n m)
sPlus SZ m = m
sPlus (SS n) m = SS (sPlus n m)

app :: Vec a n -> Vec a m -> Vec a (Plus n m)
app Nil l = l
app (Cons h t) l = Cons h (app t l)
