module Misc (
    sigmoid
) where

sigmoid :: Double -> Double
sigmoid x = 1.0 / (1.0 + exp ( negate x))



