module Delve.Events (DelveEvent(..)) where

import Delve.Scripting

data DelveEvent = SpawnPrompt String String CmdInputHandler
