module Main (main) where

import           Brick
import           Brick.BChan
import           Control.Monad

import           Delve
import           Graphics.Vty

main :: IO ()
main = do
  let loadVty = standardIOConfig >>= mkVty
  bChan <- newBChan 10
  void $ customMain loadVty (Just bChan) app ()
