module Misc (
    sigmoid
) where

sigmoid :: Floating a => a -> a
sigmoid x = 1.0 / (1.0 + exp ( negate x))



