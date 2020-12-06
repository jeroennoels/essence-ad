{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}

module Differentiation where

class Category hom where
  identity :: hom a a
  compose :: hom b c -> hom a b -> hom a c

class Category hom => Monoidal hom cross | hom -> cross where
  parallel :: hom a c -> hom b d -> hom (cross a b) (cross c d)

class Monoidal hom cross => Cartesian hom cross where
  exl :: hom (cross a b) a
  exr :: hom (cross a b) b
  dup :: hom a (cross a a) 


instance Category (->) where
  identity = id
  compose = (.)

instance Monoidal (->) (,) where
  parallel f g (a,b) = (f a, g b)

instance Cartesian (->) (,) where
  exl = fst
  exr = snd
  dup a = (a,a) 


type D a b = a -> (b, a -> b)

newtype Diff a b = Diff (D a b)

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

linear :: (a -> b) -> Diff a b
linear f = Diff (\a -> (f a, f))

instance Category Diff where
  identity = Diff dIdentity
  compose (Diff g) (Diff f) = Diff (dCompose g f)

instance Monoidal Diff (,) where
  parallel (Diff f) (Diff g) = Diff (dParallel f g) 

instance Cartesian Diff (,) where
  exl = linear fst  
  exr = linear snd
  dup = linear (\x -> (x,x))

class Monoidal hom cross => NumCat hom cross a where
  negateC :: hom a a
  addC :: hom (cross a a) a
  mulC :: hom (a,a) a

instance Num a => NumCat (->) (,) a where
  negateC = negate
  addC = uncurry (+)
  mulC = uncurry (*)

instance Num a => NumCat Diff (,) a where
  negateC = linear negate
  addC = linear (uncurry (+))
  mulC = undefined

fork :: Cartesian hom cross => hom a b -> hom a c -> hom a (cross b c)
fork f g = (parallel f g) `compose` dup

lala :: (Cartesian hom cross, NumCat hom cross a) => hom a a
lala = addC `compose` fork (negateC `compose` exl) (negateC `compose` exr) `compose` dup

lalaInstance :: Diff Double Double
lalaInstance = lala

brol = let Diff f = lalaInstance in f

-- magSqr :: Num a => (a,a) -> a
-- magSqr = addC `compose`
--   (fork
--    (mulC `compose` (fork fst fst))
--    (mulC `compose` (fork snd snd)))

