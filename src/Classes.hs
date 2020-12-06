{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
module Classes where

class Category hom where
  identity :: hom a a
  compose :: hom b c -> hom a b -> hom a c

class Category hom => Monoidal hom cross | hom -> cross where
  parallel :: hom a c -> hom b d -> hom (cross a b) (cross c d)

class Monoidal hom cross => Cartesian hom cross where
  exl :: hom (cross a b) a
  exr :: hom (cross a b) b
  dup :: hom a (cross a a)

class Monoidal hom cross => NumCat hom cross a where
  negateC :: hom a a
  addC :: hom (cross a a) a
  mulC :: hom (a,a) a
