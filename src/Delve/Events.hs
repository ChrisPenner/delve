module Delve.Events where

import System.IO

newtype DelveEvent = ScriptEvent ScriptEvent
  deriving Show

data ScriptEvent = SpawnPrompt String Handle String
  deriving Show
