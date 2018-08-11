module Graph where

type Graph g p = g -> [[Either g p]]

subg = Left  -- inject goal into goal + prim 
prim = Right -- inject prim into goal + prim
