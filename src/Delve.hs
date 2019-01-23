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
import Control.Comonad.Cofree

type ResourceName = String
type CustomEvent = ()
type AppState = FileZipper

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
drawUI fs = [renderFileZipper fs]

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
handleEvent s (VtyKey 'q' []) = halt s
handleEvent fz (VtyKey 'l' []) = continue $ descendDir fz
handleEvent fz (VtyEvent (EvKey KEnter _)) = continue $ descendDir fz
handleEvent fz (VtyKey 'h' []) = continue $ ascendDir fz

handleEvent fz@(FZ _ (x:<lst)) (VtyEvent e) = do
  lst' <- handleListEventVi (const pure) e  lst
  continue $ fz{context=(x:<lst')}
