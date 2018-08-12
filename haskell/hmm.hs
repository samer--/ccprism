module HMM (theta, graph, graphl) where

import Graph

data Prim = Obs (Int, Int) | Trans (Int, Int) deriving Show

theta (Trans _) = 0.5
theta (Obs   _) = 0.25

trans = prim . Trans
obs   = prim . Obs

graph :: [Int] -> Graph (Int, Int) Prim
graph xs = graph' where
  len = length xs
  graph' (s1,i) = if i == len then [[]] else [[trans (s1,s2), obs (s1, xs !! i), subg (s2,i+1)] | s2 <- [0, 1]]

graphl :: Graph (Int, [Int]) Prim
graphl (s1,[]) = [[]]
graphl (s1,x:xs) = [[trans (s1,s2), obs (s1, x), subg (s2,xs)] | s2 <- [0, 1]]
