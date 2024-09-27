module Control.Coprogramming (L, fix, force) where

newtype L k a = L a deriving Functor
instance Applicative (L k) where
  pure = L
  L f <*> L x = L (f x)

fix :: (L k a -> a) -> a
fix f = let x = f (L x) in x

force :: (forall k. L k a) -> a
force (L x) = x
