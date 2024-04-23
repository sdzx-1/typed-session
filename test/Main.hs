module Main (main) where

import qualified Book as Book
import qualified Book1 as Book1
main :: IO ()
main = do  
  putStrLn "----------------- run Book -----------------"
  Book.runAll
  putStrLn "---------------- run Book1 -----------------"
  Book1.runAll
  putStrLn "--------------------------------------------"
