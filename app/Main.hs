module Main
  ( main
  )
where

import           Brick

import           Delve
import           Brick.Scripting
import           Brick.Widgets.FileTree
import           Brick.BChan
import Graphics.Vty

main :: IO ()
main = do
  ft    <- newFileTree (const pure) "."
  eChan <- newBChan 10
  res   <- customMain
    (mkVty defaultConfig)
    (Just eChan)
    app
    (AppState
      { fileTree      = ft
      , eventChannel  = eChan
      , scriptingData = SD {promptData = Nothing}
      , status        = "..."
      }
    )
  putStrLn . getCurrentDir . fileTree $ res
