{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances #-}
{-# LANGUAGE ConstraintKinds, TypeFamilies #-}
module Category where

import GHC.Exts (Constraint)

class Category hom where
  type Obj hom :: * -> Constraint
  identity :: Obj hom a => hom a a
  compose :: (Obj hom a, Obj hom b, Obj hom c) => hom b c -> hom a b -> hom a c

class NoConstraint a where
instance NoConstraint a where

class Category hom => Monoidal hom cross | hom -> cross where
  parallel :: (Obj hom a, Obj hom b, Obj hom c, Obj hom d,
               Obj hom (cross a b), Obj hom (cross c d)) =>
      hom a c -> hom b d -> hom (cross a b) (cross c d)

class Monoidal hom cross => Cartesian hom cross where
  exl :: (Obj hom a, Obj hom b, Obj hom (cross a b)) => hom (cross a b) a
  exr :: (Obj hom a, Obj hom b, Obj hom (cross a b)) => hom (cross a b) b
  dup :: (Obj hom a, Obj hom (cross a a)) => hom a (cross a a)

class Monoidal hom cross => Cocartesian hom cross where
  inl :: (Obj hom a, Obj hom b, Obj hom (cross a b)) => hom a (cross a b)
  inr :: (Obj hom a, Obj hom b, Obj hom (cross a b)) => hom b (cross a b)
  jam :: (Obj hom a, Obj hom (cross a a)) => hom (cross a a) a

class Monoidal hom cross => NumCat hom cross a where
  negateC :: Obj hom a => hom a a
  addC :: (Obj hom a, Obj hom (cross a a)) => hom (cross a a) a
  mulC :: (Obj hom a, Obj hom (cross a a)) => hom (cross a a) a

class Scalable hom a where
  scale :: Obj hom a => a -> hom a a


fork :: (Obj hom a, Obj hom b, Obj hom c, Obj hom (cross a a), Obj hom (cross b c),
         Cartesian hom cross) => hom a b -> hom a c -> hom a (cross b c)
fork f g = parallel f g `compose` dup

join :: (Obj hom a, Obj hom b, Obj hom c, Obj hom (cross a b), Obj hom (cross c c),
         Cocartesian hom cross) => hom a c -> hom b c -> hom (cross a b) c
join f g = jam `compose` parallel f g
