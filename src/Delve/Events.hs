module Delve.Events where

newtype DelveEvent = ScriptEvent ScriptEvent

newtype ScriptEvent = SpawnDialog String
