module Extras (feedback) where

feedback :: (L k u -> (b k a, u)) -> b k a
feedback f = fst (fix (f . fmap snd))
