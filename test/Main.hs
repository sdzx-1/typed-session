module Main (main) where

import qualified Book
import qualified Book1
import qualified Book2
import qualified Book3
import qualified PingPong

main :: IO ()
main = do
  putStrLn "----------------- run Book -----------------"
  Book.runAll
  putStrLn "---------------- run Book1 -----------------"
  Book1.runAll
  putStrLn "---------------- run Book2 -----------------"
  Book2.runAll
  putStrLn "---------------- run Book3 -----------------"
  Book3.runAll
  putStrLn "---------------- run PingPong -----------------"
  PingPong.runAll
  putStrLn "--------------------------------------------"
