module Delve.Commands (openInVim) where

import Brick
import Brick.Widgets.FileTree

import Control.Monad.IO.Class

import Data.Maybe

import System.Posix.Process

openInVim :: FileTree a -> EventM r ()
openInVim ft =
  do
    let f = getCurrentFilePath ft
    liftIO $ executeFile "vim" True (maybeToList f) Nothing
