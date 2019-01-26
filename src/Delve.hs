{-# LANGUAGE PatternSynonyms #-}
module Delve (app) where

import           Brick
import           Brick.Widgets.List
import           Graphics.Vty.Input.Events
import           Graphics.Vty.Attributes
import Brick.Widgets.FileTree
import Brick.Scripting
import Brick.Widgets.Border
import System.Posix.Process
import Control.Monad.IO.Class
import Data.Maybe

type ResourceName = String
type CustomEvent = ()
type AppState = (FileTree FilePath)

app :: App AppState CustomEvent ResourceName
app = App
  { appDraw         = drawUI
  , appChooseCursor = chooseCursor
  , appHandleEvent  = handleEvent
  , appStartEvent   = pure
  , appAttrMap      = const attrs
  }

attrs :: AttrMap
attrs = attrMap defAttr [ (dirAttr, cyan `on` black)
                        , (listSelectedAttr, black `on` white)
                        , (listSelectedFocusedAttr, black `on` green)
                        , (borderAttr, green `on` black)
                        , (flaggedItemAttr, yellow `on` black)
                        , (titleAttr, green `on` black)
                        ]

drawUI :: AppState -> [Widget ResourceName]
drawUI fs = [renderFileTreeCustom renderFileContext fs]

chooseCursor
  :: AppState
  -> [CursorLocation ResourceName]
  -> Maybe (CursorLocation ResourceName)
chooseCursor _ [] = Nothing
chooseCursor _ (c:_) = Just c

pattern VtyKey :: Char -> [Modifier] -> BrickEvent n e
pattern VtyKey k mods = VtyEvent (EvKey (KChar k) mods)

handleEvent
  :: AppState
  -> BrickEvent ResourceName CustomEvent
  -> EventM ResourceName (Next AppState)
handleEvent fz (VtyKey 'c' [MCtrl]) = halt fz
handleEvent fz (VtyKey 'q' []) = halt fz
handleEvent fz (VtyKey '-' []) = continue $ toggleFlaggedVisible fz
handleEvent fz (VtyKey 'l' []) =  descendDir fz >>= continue
handleEvent fz (VtyKey ' ' []) = toggleFlagged fz >>= continue
handleEvent fz (VtyEvent (EvKey KEnter _)) = do
  let f = getCurrentFilePath fz
  liftIO $ executeFile "vim" True (maybeToList f) Nothing
handleEvent fz (VtyKey 'h' []) =  ascendDir fz >>= continue
handleEvent fz (VtyKey 'j' []) = moveDown fz >>= continue
handleEvent fz (VtyKey 'k' []) = moveUp fz >>= continue
handleEvent fz (VtyKey 'o' []) = simpleCommand fz "scr" >> continue fz
handleEvent fz _ = continue fz
