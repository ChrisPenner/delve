module Main
  ( main
  )
where

import           Brick
import           Brick.Widgets.FileBrowser
import           Brick.BChan
import           Control.Monad

import           Delve
import Delve.Actions
import           Graphics.Vty
import           Data.List.NonEmpty

main :: IO ()
main = do
  let loadVty = standardIOConfig >>= mkVty
  bChan <- newBChan 10
  fb    <- buildTree "."
  void $ customMain loadVty (Just bChan) app fb
