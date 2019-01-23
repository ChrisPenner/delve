{-# LANGUAGE PatternSynonyms #-}
module Delve (app) where

import           Brick
import           Brick.Widgets.FileBrowser
import           Brick.Widgets.List
import           Graphics.Vty.Input.Events
import           Graphics.Vty.Attributes
import Brick.FileTree.Actions
import Brick.FileTree.Types
import Brick.FileTree.Render
import Control.Comonad.Cofree
import System.Posix.Process
import Control.Monad.IO.Class
import Data.Maybe

type ResourceName = String
type CustomEvent = ()
type AppState = FileTree

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
handleEvent fz (VtyKey 'l' []) =  descendDir fz >>= continue
-- handleEvent fz (VtyEvent (EvKey KEnter _)) = halt fz
handleEvent fz (VtyEvent (EvKey KEnter _)) = do
  let f = select fz
  liftIO $ executeFile "vim" True (maybeToList f) Nothing
handleEvent fz (VtyKey 'h' []) =  ascendDir fz >>= continue
handleEvent fz@(FZ _ (x:<lst)) (VtyEvent e) = do
  lst' <- handleListEventVi (const pure) e  lst
  continue $ fz{context=x:<lst'}
handleEvent fz _ = continue fz
