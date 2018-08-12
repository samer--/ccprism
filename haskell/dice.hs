module Dice where

import Control.Monad
import Graph

die = prim

possible (k,n) = k <= n && n <= 4*k

graph :: Graph (Int, Int) Int
graph (0,0) = [[]]
graph (k,z) | k > 0 = [[subg g, die n] | n <- [1..4], let g = (k-1,z-n), possible g]

theta :: Fractional t => a -> t
theta = const (1/4)

theta2 = ([0, 0.2, 0.4, 0.3, 0.1] !!)

-- monadic expression of dice generative model
dice 0 = return 0
dice k | k > 0 = do
  x <- uniform [1..4]
  y <- dice (k-1)
  return (x + y)

uniform = id -- just use list monad for now
