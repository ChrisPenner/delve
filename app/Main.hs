module Main
  ( main
  )
where

import           Brick

import           Delve
import           Brick.Scripting
import           Brick.Widgets.FileTree
import           Brick.BChan
import           Brick.Widgets.Edit
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
      , scriptingData = SD {prompt = Just (editor "hi" (Just 1) "testing:)")}
      }
    )
  putStrLn . getCurrentDir . fileTree $ res
