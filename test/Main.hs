{-# LANGUAGE TypeApplications #-}
module Main (main) where

import Book3.Main
import Control.Monad
import System.Random
import Control.Exception

main :: IO ()
main =  do
  replicateM_ 100 $ do
    g <- newStdGen
    case book3Prop g of
      Left e -> throwIO e
      Right _ -> pure ()
