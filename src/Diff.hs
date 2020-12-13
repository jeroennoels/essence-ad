{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, FlexibleContexts #-}
{-# LANGUAGE ConstraintKinds, TypeFamilies #-}
module Diff (Diff(Diff)) where

import Category
import Additive
import AddFun

type D a b = a -> (b, AddFun a b)

dIdentity :: Additive a => D a a
dIdentity a = (a, identity)

dCompose :: (Additive a, Additive b, Additive c) =>
    D b c -> D a b -> D a c
dCompose g f a = let
  (b, f') = f a
  (c, g') = g b
  in (c, compose g' f')

dParallel :: (Additive a, Additive b, Additive c, Additive d) =>
    D a c -> D b d -> D (a,b) (c,d)
dParallel f g (a,b) = let
  (c, f') = f a
  (d, g') = g b
  in ((c,d), parallel f' g')

newtype Diff a b = Diff (D a b)

linear :: (Additive a, Additive b) => (a -> b) -> Diff a b
linear f = Diff $ \a -> (f a, AddFun f)

instance Category Diff where
  type Obj Diff = Additive
  identity = Diff dIdentity
  compose (Diff g) (Diff f) = Diff (dCompose g f)

instance Monoidal Diff (,) where
  parallel (Diff f) (Diff g) = Diff (dParallel f g)

instance Cartesian Diff (,) where
  exl = linear fst
  exr = linear snd
  dup = linear $ \x -> (x,x)

instance Num a => Scalable AddFun a where
  scale a = AddFun (a *)

privateMulC :: (Num a, Scalable AddFun a, Additive a) => D (a,a) a
privateMulC (a,b) = (a * b, scale b `join` scale a)

instance Num a => NumCat Diff (,) a where
  negateC = linear negate
  addC = linear (uncurry (+))
  mulC = Diff privateMulC
