module Delve.State (AppState(..)) where

import Brick.BChan
import Brick.Widgets.Edit
import Brick.Widgets.FileTree

import Delve.Events

import GHC.Generics
import GHC.IO.Handle

data AppState =
  AppState
    { fileTree     :: FileTree FilePath
    , eventChannel :: BChan DelveEvent
    , prompt       :: Maybe (Editor String String, Handle)
    , status       :: String
    }
    deriving Generic
