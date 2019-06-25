{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PatternSynonyms #-}

module Delve (app,AppState(..)) where

import Brick
import Brick.BChan
import Brick.Widgets.Border
import Brick.Widgets.Edit
import Brick.Widgets.FileTree
import Brick.Widgets.List

import Control.Lens
import Control.Monad
import Control.Monad.IO.Class

import Data.Generics.Product
import qualified Data.Map as M
import Data.Maybe

import Delve.Commands
import Delve.Events
import Delve.ScriptDialog
import Delve.Scripting
import Delve.State

import Graphics.Vty.Attributes
import Graphics.Vty.Input.Events

type ResourceName = String

_prompt :: Lens' AppState (Maybe (Editor String String, CmdInputHandler))
_prompt = field @"prompt"

app :: App AppState DelveEvent ResourceName
app =
  App
  { appDraw         = drawUI
  , appChooseCursor = chooseCursor
  , appHandleEvent  = interceptPromptEvents
  , appStartEvent   = pure
  , appAttrMap      = const attrs
  }


attrs :: AttrMap
attrs =
  attrMap
    defAttr
    [ (dirAttr, cyan `on` black)
    , (listSelectedAttr, black `on` white)
    , (listSelectedFocusedAttr, black `on` green)
    , (borderAttr, green `on` black)
    , (flaggedItemAttr, yellow `on` black)
    , (titleAttr, green `on` black)
    ]

drawUI :: AppState -> [Widget ResourceName]
drawUI s =
  [ renderPrompt (prompt s)
  , renderFileTreeCustom renderFileContext (s ^. _fileTree)
      <=> hBorder
      <=> str (status s)
  ]

chooseCursor :: AppState
             -> [CursorLocation ResourceName]
             -> Maybe (CursorLocation ResourceName)
chooseCursor _ [] = Nothing
chooseCursor _ (c:_) = Just c

pattern VtyKey :: Char -> [Modifier] -> BrickEvent n e
pattern VtyKey k mods = VtyEvent (EvKey (KChar k) mods)

_fileTree :: Lens' AppState (FileTree FilePath)
_fileTree = field @"fileTree"

interceptPromptEvents :: AppState
                      -> BrickEvent ResourceName DelveEvent
                      -> EventM ResourceName (Next AppState)
interceptPromptEvents s (VtyEvent e)
  | isJust (prompt s) = (_prompt %%~ handlePromptEvent e) s >>= continue
interceptPromptEvents s e = handleEvent e s

handleEvent :: BrickEvent ResourceName DelveEvent
            -> AppState
            -> EventM ResourceName (Next AppState)
handleEvent (VtyKey 'c' [MCtrl]) = halt
handleEvent (VtyKey 'q' []) = halt
handleEvent (VtyKey '-' []) = continue . (_fileTree %~ toggleFlaggedVisible)
handleEvent (VtyKey 'l' []) = _fileTree %%~ descendDir >=> continue
handleEvent (VtyKey ' ' []) = _fileTree %%~ toggleFlagged >=> continue
handleEvent (VtyEvent (EvKey KEnter _)) =
  \s -> do
    openInVim (s ^. _fileTree)
    continue s
handleEvent (VtyKey 'h' []) = _fileTree %%~ ascendDir >=> continue
handleEvent (VtyKey 'j' []) = _fileTree %%~ moveDown >=> continue
handleEvent (VtyKey 'k' []) = _fileTree %%~ moveUp >=> continue
handleEvent (VtyKey 'o' []) =
  \s -> do
    void . liftIO
      $ spawnCmd
        "scr"
        (treeContext . fileTree $ s)
        (responseToChannel (eventChannel s))
    continue s
  where
    responseToChannel :: BChan DelveEvent -> CmdOutputHandler
    responseToChannel eChan responder respStr =
      do
        appendFile "log" (respStr ++ "\n")
        writeBChan eChan (SpawnPrompt "scr" respStr responder)

    treeContext ft =
      M.fromList
        [ (flaggedKey, unlines $ getFlagged ft)
        , (focusedKey, fromMaybe "" $ getCurrentFilePath ft)
        , (currentDirKey, getCurrentDir ft)
        ]
handleEvent (AppEvent e) = (_prompt %%~ handleScriptEvents e) >=> continue
handleEvent _ = continue

flaggedKey :: String
flaggedKey = "DELVE_FLAGGED"

focusedKey :: String
focusedKey = "DELVE_FOCUSED"

currentDirKey :: String
currentDirKey = "DELVE_CURRENT_DIR"
