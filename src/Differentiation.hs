{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Differentiation where

import Classes
import Fun
import Diff

fork :: Cartesian hom cross => hom a b -> hom a c -> hom a (cross b c)
fork f g = parallel f g `compose` dup

timesMinusTwo :: (Cartesian hom cross, NumCat hom cross a) => hom a a
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

