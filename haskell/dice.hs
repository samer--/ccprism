module Dice where

import Graph

die = prim

possible (k,n) = k <= n && n <= 4*k

graph :: Graph (Int, Int) Int
graph (0,0) = [[]]
graph (k,z) | k > 0 = [[subg g, die n] | n <- [1..4], let g = (k-1,z-n), possible g]

theta :: Fractional t => a -> t
theta = const (1/4)

theta2 = ([0, 0.2, 0.1, 0.3, 0.4] !!)
