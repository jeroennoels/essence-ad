{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

-- Not exporting anything for now, this is just an example.
module Fun () where

import Classes

-- To avoid confusion I prefer the categorical language does not work directly
-- with Haskell's build-in type constructors.
newtype Fun a b = Fun (a -> b)
data Pair a b = Pair a b

instance Category Fun where
  identity = Fun id
  compose (Fun g) (Fun f) = Fun (g . f)

instance Monoidal Fun Pair where
  parallel (Fun f) (Fun g) = Fun $
    \(Pair a b) -> Pair (f a) (g b)

instance Cartesian Fun Pair where
  exl = Fun $ \(Pair a _) -> a
  exr = Fun $ \(Pair _ b) -> b
  dup = Fun $ \a -> Pair a a

instance Num a => NumCat Fun Pair a where
  negateC = Fun negate
  addC = Fun $ \(Pair a b) -> a + b
  mulC = Fun $ \(Pair a b) -> a * b
