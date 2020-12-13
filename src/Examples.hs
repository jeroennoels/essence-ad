module Examples where

import Category
import Additive
import AddFun
import Diff

instance Additive Double where
  zero = 0
  add = (+)

magSqr ::(Num a, Additive a) => Diff (a,a ) a
magSqr = addC `compose`
  (fork
   (mulC `compose` (fork exl exl))
   (mulC `compose` (fork exr exr)))

example x = (y, f' x)
  where
    Diff f = magSqr
    (y, AddFun f') = f x
