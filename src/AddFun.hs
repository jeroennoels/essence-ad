{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}
{-# LANGUAGE ConstraintKinds, TypeFamilies #-}
module AddFun () where

import Category

newtype AddFun a b = AddFun (a -> b)

instance Category AddFun where
  type Obj AddFun = Additive
  identity = AddFun id
  compose (AddFun g) (AddFun f) = AddFun (g . f)

instance Monoidal AddFun (,) where
  parallel (AddFun f) (AddFun g) = AddFun $ \(a,b) -> (f a, g b)

instance Cartesian AddFun (,) where
  exl = AddFun fst
  exr = AddFun snd
  dup = AddFun $ \a -> (a,a)

instance (Additive a, Additive b) => Additive (a,b) where
  zero = (zero, zero)
  add (a,b) (c,d) = (add a c, add b d)

instance Cocartesian AddFun (,) where
  inl = AddFun $ \a -> (a, zero)
  inr = AddFun $ \a -> (zero, a)
  jam = AddFun $ uncurry add
