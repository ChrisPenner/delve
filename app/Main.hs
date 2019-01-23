module Main
  ( main
  )
where

import           Brick
import           Brick.BChan
import           Control.Monad
import Data.Maybe

import           Delve
import Delve.Actions
import           Graphics.Vty

main :: IO ()
main = do
  let loadVty = standardIOConfig >>= mkVty
  bChan <- newBChan 10
  fb    <- buildTree "."
  res   <- customMain loadVty (Just bChan) app fb
  putStrLn $ fromMaybe "" $ select res
