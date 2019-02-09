{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

module Delve.ScriptDialog (handlePromptEvent,renderPrompt,handleScriptEvents) where

import Brick.AttrMap
import Brick.Types
import Brick.Widgets.Border
import Brick.Widgets.Core
import Brick.Widgets.Edit

import Control.Lens as L
import Control.Monad.IO.Class

import Delve.Events
import Delve.State

import GHC.IO.Handle

import qualified Graphics.Vty.Input.Events as V

flaggedKey :: String
flaggedKey = "DELVE_FLAGGED"

focusedKey :: String
focusedKey = "DELVE_FOCUSED"

currentDirKey :: String
currentDirKey = "DELVE_CURRENT_DIR"

scriptOverlayBGAttr :: AttrName
scriptOverlayBGAttr = "script-overlay-bg"

renderPrompt :: Maybe (Editor String String, Handle) -> Widget String
renderPrompt Nothing = emptyWidget
renderPrompt (Just (e,_)) =
  withAttr scriptOverlayBGAttr
  $ padTopBottom 2
  $ renderEditor (vBox . fmap str) True e <=> hBorder

handleScriptEvents :: ScriptEvent
                   -> Maybe (Editor String String, Handle)
                   -> EventM r (Maybe (Editor String String, Handle))
handleScriptEvents (SpawnPrompt cmd promptResponseH contents) _ =
  let numLines = Just 1
      eName    = cmd
      edt      = editor eName numLines contents
  in return $ Just (edt, promptResponseH)

handlePromptEvent :: V.Event
                  -> Maybe (Editor String String, Handle)
                  -> EventM String (Maybe (Editor String String, Handle))
handlePromptEvent (V.EvKey V.KEnter _) s =
  do
    case s of
      Nothing -> return ()
      Just (getEditContents -> txtLines,h)
        -> liftIO . hPutStr h $ unlines txtLines
    return Nothing
handlePromptEvent e s = ((_Just . _1) %%~ handleEditorEvent e) s
