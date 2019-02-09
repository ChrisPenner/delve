module Main (main) where

import Brick
import Brick.BChan
import Brick.Widgets.FileTree

import Delve

import Graphics.Vty

main :: IO ()
main =
  do
    ft <- newFileTree (const pure) "."
    eChan <- newBChan 10
    res <- customMain
      (mkVty defaultConfig)
      (Just eChan)
      app
      (AppState
       { fileTree     = ft
       , eventChannel = eChan
       , status       = "..."
       , prompt       = Nothing
       })
    putStrLn . getCurrentDir . fileTree $ res
