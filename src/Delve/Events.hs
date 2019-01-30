module Delve.Events where

newtype DelveEvent = ScriptEvent ScriptEvent

data ScriptEvent = SpawnDialog String String
