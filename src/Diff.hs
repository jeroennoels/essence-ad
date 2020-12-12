{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}
{-# LANGUAGE ConstraintKinds, TypeFamilies #-}
module Diff (Diff(Diff)) where

import Category

type D a b = a -> (b, a -> b)

dIdentity :: D a a
dIdentity a = (a, id)

dCompose :: D b c -> D a b -> D a c
dCompose g f a = let
  (b, f') = f a
  (c, g') = g b
  in (c, g' . f')

dParallel :: D a c -> D b d -> D (a,b) (c,d)
dParallel f g (a,b) = let
  (c, f') = f a
  (d, g') = g b
  h (x,y) = (f' x, g' y)
  in ((c,d), h)


newtype Diff a b = Diff (D a b)

linear :: (a -> b) -> Diff a b
linear f = Diff $ \a -> (f a, f)

instance Category Diff where
  type Obj Diff = NoConstraint
  identity = Diff dIdentity
  compose (Diff g) (Diff f) = Diff (dCompose g f)

instance Monoidal Diff (,) where
  parallel (Diff f) (Diff g) = Diff (dParallel f g)

instance Cartesian Diff (,) where
  exl = linear fst
  exr = linear snd
  dup = linear $ \x -> (x,x)

instance Num a => NumCat Diff (,) a where
  negateC = linear negate
  addC = linear (uncurry (+))
  mulC = undefined
