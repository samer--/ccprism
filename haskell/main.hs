module Main where

import System.Environment
import System.TimeIt
import Data.Ratio
import Dice as Dice
import HMM as HMM
import Cata3

main = getArgs >>= time

time :: [String] -> IO ()
time (cmd:args) = do
  let [k, n] = map read args
  (t,_) <- timeItT $ test cmd (k,n)
  putStrLn $ "Time: " ++ show t

test "vtree5"  = print . cata (viterbi_tree_log_float Dice.theta) Dice.graph
test "vtree6"  = print . mcata (viterbi_tree_log_float Dice.theta) Dice.graph
test "inside6" = print . mcata (inside Dice.theta) Dice.graph
test "hmm"     = \(k,n) -> print $ mcata (inside_log HMM.theta) HMM.graphl (0, take k $ repeat n)
test "hmm_int" = \(k,n) -> print $ mcata (inside_log HMM.theta) (HMM.graph $ take k $ repeat n) (0,0)
