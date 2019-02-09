{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}

module Brick.Scripting (spawnCmd) where

import Brick.BChan
import Brick.Types

import Control.Concurrent
import Control.Concurrent.Async
import Control.Exception
import Control.Monad
import Control.Monad.IO.Class

import Data.Bifoldable
import qualified Data.Map as M

import Delve.Events

import GHC.IO.Handle.FD

import qualified Streaming.Prelude as SP

import System.IO
import System.Process

type CmdEnv = M.Map String String

spawnPromptPipeKey :: String
spawnPromptPipeKey = "DELVE_SPAWN_PROMPT"

promptResponsePipeKey :: String
promptResponsePipeKey = "DELVE_PROMPT_RESPONSE"

type Pipe = (Handle, Handle)

spawnCmd :: BChan DelveEvent -> String -> CmdEnv -> EventM String ()
spawnCmd eChan cmd cmdEnv =
  void . liftIO . forkOS
  $ bracket
    acquirePipeDescriptors
    releasePipeDescriptors
    (spawnCmd' eChan cmd cmdEnv)

acquirePipeDescriptors :: IO (Pipe, Pipe)
acquirePipeDescriptors =
  do
    spawnPromptPipe <- createPipe
    promptResponsePipe <- createPipe
    let lnBuffer = (`hSetBuffering` LineBuffering)
    bitraverse_ lnBuffer lnBuffer spawnPromptPipe
    bitraverse_ lnBuffer lnBuffer promptResponsePipe
    return (spawnPromptPipe, promptResponsePipe)

releasePipeDescriptors :: (Pipe, Pipe) -> IO ()
releasePipeDescriptors (pa,pb) =
  do
    bitraverse_ hClose hClose pa
    bitraverse_ hClose hClose pb

pipeOutput :: (Handle, Handle) -> Handle
pipeOutput = fst

pipeInput :: (Handle, Handle) -> Handle
pipeInput = snd

spawnCmd' :: BChan DelveEvent -> String -> CmdEnv -> (Pipe, Pipe) -> IO ()
spawnCmd' eventChannel cmdStr cmdEnv (spawnPromptPipe,promptResponsePipe) =
  do
    spawnPromptInFD <- show <$> handleToFd (pipeInput spawnPromptPipe)
    promptResponseOutFD <- show <$> handleToFd (pipeOutput promptResponsePipe)
    (_,_,_,procHandle) <- withFile "/dev/null" WriteMode
      $ \devNull -> do
        createProcess
          (proc cmdStr [])
            { env       = Just
                $ M.toList cmdEnv
                <> [ (spawnPromptPipeKey, spawnPromptInFD)
                   , (promptResponsePipeKey, promptResponseOutFD)]
            , std_in    = UseHandle devNull
            , std_out   = UseHandle devNull
            , std_err   = UseHandle devNull
            , close_fds = False
            }
    let promptWorker =
          promptWatcher cmdStr spawnPromptPipe promptResponsePipe eventChannel
    void $ withAsync promptWorker $ const (waitForProcess procHandle)

promptWatcher :: String -> Pipe -> Pipe -> BChan DelveEvent -> IO ()
promptWatcher cmdStr spawnPromptPipe promptResponsePipe eChan =
  do
    SP.effects
      (SP.mapM
         (sendToChan
          . ScriptEvent
          . SpawnPrompt cmdStr (pipeInput promptResponsePipe))
       $ SP.fromHandle (pipeOutput spawnPromptPipe))
  where
    sendToChan :: DelveEvent -> IO ()
    sendToChan de = writeBChan eChan de
