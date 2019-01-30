{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DeriveGeneric #-}
module Delve (app, AppState(..)) where

import Control.Lens
import           Brick
import           Brick.BChan
import           Brick.Widgets.List
import           Graphics.Vty.Input.Events
import           Graphics.Vty.Attributes
import Brick.Widgets.FileTree
import Brick.Scripting
import Brick.Widgets.Border
import Delve.Events
import Data.Generics.Product
import Control.Monad
import GHC.Generics
import Brick.Widgets.Edit

type ResourceName = String
data AppState =
  AppState
    { fileTree :: FileTree FilePath
    , eventChannel :: BChan DelveEvent
    , scriptingData :: ScriptingData
    } deriving Generic

_scriptingData
  :: Lens' AppState ScriptingData 
_scriptingData = field @"scriptingData"

app :: App AppState DelveEvent ResourceName
app = App
  { appDraw         = drawUI
  , appChooseCursor = chooseCursor
  , appHandleEvent  = flip handleEvent
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
drawUI s = [renderScripting (s^._scriptingData), renderFileTreeCustom renderFileContext (s^._fileTree)]

chooseCursor
  :: AppState
  -> [CursorLocation ResourceName]
  -> Maybe (CursorLocation ResourceName)
chooseCursor _ [] = Nothing
chooseCursor _ (c:_) = Just c

pattern VtyKey :: Char -> [Modifier] -> BrickEvent n e
pattern VtyKey k mods = VtyEvent (EvKey (KChar k) mods)

_fileTree :: Lens' AppState (FileTree FilePath)
_fileTree = field @"fileTree"

handleEvent
  :: BrickEvent ResourceName DelveEvent
  -> AppState
  -> EventM ResourceName (Next AppState)
handleEvent (VtyKey 'c' [MCtrl]) = halt
handleEvent (VtyKey 'q' []) = halt
handleEvent (VtyKey '-' []) = continue . (_fileTree %~ toggleFlaggedVisible)
handleEvent (VtyKey 'l' []) =  _fileTree %%~ descendDir >=> continue
handleEvent (VtyKey ' ' []) = _fileTree %%~ toggleFlagged >=> continue
handleEvent (VtyEvent (EvKey KEnter _)) = \s -> do
  openInVim (s ^. _fileTree )
  continue s
handleEvent (VtyKey 'h' []) = _fileTree %%~ ascendDir >=> continue
handleEvent (VtyKey 'j' []) = _fileTree %%~ moveDown >=> continue
handleEvent (VtyKey 'k' []) = _fileTree %%~ moveUp  >=> continue
handleEvent (VtyKey 'o' []) = \s -> do handleCmd (eventChannel s) "scr" >> continue s
handleEvent _ = continue
