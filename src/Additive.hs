module Additive where

class Additive a where
  zero :: a
  add :: a -> a -> a

instance (Additive a, Additive b) => Additive (a,b) where
  zero = (zero, zero)
  add (a,b) (c,d) = (add a c, add b d)
