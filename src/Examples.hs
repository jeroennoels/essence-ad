module Examples where

import Category
import Diff

timesMinusTwo = addC
  `compose` fork (negateC `compose` exl) (negateC `compose` exr)
  `compose` dup

example :: Double -> (Double, Double -> Double)
example = let Diff f = timesMinusTwo in f

-- magSqr :: Num a => (a,a) -> a
-- magSqr = addC `compose`
--   (fork
--    (mulC `compose` (fork fst fst))
--    (mulC `compose` (fork snd snd)))
