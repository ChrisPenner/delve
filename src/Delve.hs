{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}
module Delve (app) where

import           Brick
import           Brick.Widgets.FileBrowser
import           Brick.Widgets.List
import           Graphics.Vty.Input.Events
import           Graphics.Vty.Attributes
import Data.List.NonEmpty
import Control.Monad.IO.Class
import Delve.Actions

type ResourceName = String
type CustomEvent = ()
type AppState = FSLayer

app :: App AppState CustomEvent ResourceName
app = App
  { appDraw         = drawUI
  , appChooseCursor = chooseCursor
  , appHandleEvent  = handleEvent
  , appStartEvent   = pure
  , appAttrMap      = const attrs
  }

attrs :: AttrMap
attrs = attrMap defAttr [ (fileBrowserSelectedAttr, red `on` black)
                        , (listSelectedFocusedAttr, black `on` white)
                        ]

drawUI :: AppState -> [Widget ResourceName]
drawUI fs = [renderFSLayer fs]

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
handleEvent s (VtyKey 'c' [MCtrl]) = halt s
handleEvent s (VtyEvent (EvKey KEnter _)) = do
  undefined
  -- case fileBrowserCursor fb of
  --   Nothing -> continue s
  --   Just (fileInfoFilename -> "..") -> do
  --     continue (fromList rest)
  --   Just (fileInfoFilePath -> fp) -> do
  --     newFb <- liftIO $ newFileBrowser (const True) (Prelude.length s) (Just fp)
  --     continue (newFb :| fb : rest)
handleEvent s _ = continue s
