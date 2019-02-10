module Delve.State (AppState(..)) where

import Brick.BChan
import Brick.Widgets.Edit
import Brick.Widgets.FileTree

import Delve.Events
import Delve.Scripting

import GHC.Generics

data AppState =
  AppState
    { fileTree     :: FileTree FilePath
    , eventChannel :: BChan DelveEvent
    , prompt       :: Maybe (Editor String String, CmdInputHandler)
    , status       :: String
    }
    deriving Generic
