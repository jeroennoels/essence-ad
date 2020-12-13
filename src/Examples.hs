module Examples where

import Category
import Additive
import AddFun
import Diff

instance Additive Double where
  zero = 0
  add = (+)

sqr :: (Num a, Additive a) => Diff a a
sqr = mulC `compose` dup

magSqr :: (Num a, Additive a) => Diff (a,a) a
magSqr = addC `compose` parallel sqr sqr

power8 :: (Num a, Additive a) => Diff a a
power8 = sqr `compose` sqr `compose` sqr

example x = (y, f' 1)
  where
    Diff f = power8
    (y, AddFun f') = f x
