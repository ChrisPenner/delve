module Main
  ( main
  )
where

import           Brick

import           Delve
import           Brick.Widgets.FileTree
import           Brick.BChan

main :: IO ()
main = do
  ft    <- newFileTree (const pure) "."
  eChan <- newBChan 10
  res   <- defaultMain app (AppState {fileTree = ft, eventChannel = eChan})
  putStrLn . getCurrentDir . fileTree $ res
