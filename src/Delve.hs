{-# LANGUAGE PatternSynonyms #-}
module Delve (app) where

import           Brick
import           Graphics.Vty.Input.Events
import           Graphics.Vty.Attributes

type ResourceName = String
type CustomEvent = ()
type AppState = ()

app :: App AppState CustomEvent ResourceName
app = App
  { appDraw         = drawUI
  , appChooseCursor = chooseCursor
  , appHandleEvent  = handleEvent
  , appStartEvent   = pure
  , appAttrMap      = const attrs
  }

attrs :: AttrMap
attrs = attrMap defAttr []

drawUI :: AppState -> [Widget ResourceName]
drawUI _ = []

chooseCursor
  :: AppState
  -> [CursorLocation ResourceName]
  -> Maybe (CursorLocation ResourceName)
chooseCursor _ _ = Nothing

pattern VtyKey :: Char -> BrickEvent n e
pattern VtyKey k = VtyEvent (EvKey (KChar k) [])

handleEvent
  :: AppState
  -> BrickEvent ResourceName CustomEvent
  -> EventM ResourceName (Next AppState)
handleEvent s (VtyEvent (EvKey (KChar 'c') [MCtrl])) = halt s
handleEvent s (VtyEvent (EvResize _ _)) = invalidateCache >> continue s
handleEvent s (VtyKey 'j') = continue s
handleEvent s _ = continue s

-- mainLoop :: 
