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
import Delve.Scripting

import qualified Graphics.Vty.Input.Events as V

scriptOverlayBGAttr :: AttrName
scriptOverlayBGAttr = "script-overlay-bg"

renderPrompt :: Maybe (Editor String String, CmdInputHandler) -> Widget String
renderPrompt Nothing = emptyWidget
renderPrompt (Just (e,_)) =
  withAttr scriptOverlayBGAttr
  $ padTopBottom 2
  $ renderEditor (vBox . fmap str) True e <=> hBorder

handleScriptEvents :: DelveEvent
                   -> Maybe (Editor String String, CmdInputHandler)
                   -> EventM r (Maybe (Editor String String, CmdInputHandler))
handleScriptEvents (SpawnPrompt cmd contents responseHandler) _ =
  let numLines = Just 1
      eName    = cmd
      edt      = editor eName numLines contents
  in do
       liftIO (appendFile "log" "spawning editor\n")
       return $ Just (edt, responseHandler)

handlePromptEvent
  :: V.Event
  -> Maybe (Editor String String, CmdInputHandler)
  -> EventM String (Maybe (Editor String String, CmdInputHandler))
handlePromptEvent (V.EvKey V.KEnter _) s =
  do
    case s of
      Nothing -> return ()
      Just (getEditContents -> txtLines,handler)
        -> liftIO . handler $ unlines txtLines
    return Nothing
handlePromptEvent e s = ((_Just . _1) %%~ handleEditorEvent e) s
