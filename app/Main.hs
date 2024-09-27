{-# LANGUAGE MultiWayIf #-}
{-# OPTIONS_GHC -fplugin=LiquidHaskell #-}
module Main where

import Control.Coprogramming

data Tree a = Leaf a | Br (Tree a) (Tree a) deriving Show

-- xrepmin :: forall a. Ord a => Tree a -> Tree a
-- xrepmin t = let (res, m) = xrepmin' t m in res
-- 
-- xrepmin' :: Ord a => Tree a -> a -> (Tree a, a)
-- xrepmin' (Leaf y) m = (Leaf m, y)
-- xrepmin' (Br l r) m =
--   let (l', m1) = xrepmin' l m
--       (r', m2) = xrepmin' r m
--   in (Br l' r', min m1 m2)

repmin :: forall a. Ord a => Tree a -> Tree a
repmin t = force (fst (fix (repmin' t . fmap snd)))

repmin' :: Ord a => Tree a -> L s a -> (L s (Tree a), a)
repmin' (Leaf x) m = (Leaf <$> m, x)
repmin' (Br l r) m =
  let (l', m1) = repmin' l m
      (r', m2) = repmin' r m
  in (Br <$> l' <*> r', min m1 m2)

ex :: Tree Integer
ex = Br (Br (Leaf 5) (Br (Leaf 2) (Leaf 3))) (Leaf 4)

data Stream a = Cons a (Stream a)

hd :: Stream a -> a
hd (Cons x _) = x
tl :: Stream a -> Stream a
tl (Cons _ xs) = xs

onHd :: (a -> a) -> Stream a -> Stream a
onHd f (Cons x xs) = Cons (f x) xs

data LStream s a = LCons a (L s (LStream s a))
lhd :: LStream s a -> a
lhd (LCons x _) = x
ltl :: LStream s a -> L s (LStream s a)
ltl (LCons _ xs) = xs
lonHd :: (a -> a) -> LStream s a -> LStream s a
lonHd f (LCons x xs) = LCons (f x) xs
lonTl :: (LStream s a -> (b, LStream s a)) -> LStream s a -> (L s b, LStream s a)
lonTl f (LCons x xs) = let (y, ls') = lunzip (f <$> xs) in (y, LCons x ls')

lunzip :: L s (a, b) -> (L s a, L s b)
lunzip x = (fst <$> x, snd <$> x)

-- bfl :: Tree a -> Tree Integer
-- bfl t = fst (fix (bfl' t . LCons 0 . fmap snd))
-- 
-- bfl' :: Tree a -> LStream s Integer -> ((Tree Integer), LStream s Integer)
-- bfl' (Leaf _) ls = (Leaf $ lhd ls, lonHd (+ 1) ls)
-- bfl' (Br l r) ls = _ $
--   lonTl
--     (\ls' ->
--       let (l', ls'') = bfl' l ls'
--           (r', ls''') = bfl' r ls''
--       in (Br l' r', ls'''))
--     ls

gcd :: Int -> Int -> Int
gcd a0 b0 = force $ fix (\gcd' a b -> 
  if | a == b -> pure a
     | a >  b -> fmap (\gcd'' -> gcd'' (a - b) b) gcd' 
     | a <  b -> _ gcd' a (b - a) 
  ) a0 b0


main :: IO ()
main = do
  print (repmin ex)
--  print (bfl ex)
