module Main
  ( main
  )
where

import           Brick
import           Brick.BChan

import           Delve
import           Brick.FileTree
import           Graphics.Vty

main :: IO ()
main = do
  let loadVty = standardIOConfig >>= mkVty
  bChan <- newBChan 10
  fb    <- newFileTree "."
  res   <- customMain loadVty (Just bChan) app fb
  putStrLn $ getCurrentDir res
