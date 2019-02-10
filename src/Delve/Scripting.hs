{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module Delve.Scripting (spawnCmd,CmdOutputHandler,CmdInputHandler) where

import Control.Concurrent
import Control.Concurrent.Async
import Control.Monad

import Data.Bifoldable
import qualified Data.Map as M

import GHC.IO.Handle.FD

import qualified Streaming.Prelude as SP

import System.IO
import System.Process

type CmdOutputHandler = CmdInputHandler -> String -> IO ()

type CmdInputHandler = String -> IO ()

type CmdEnv = M.Map String String

spawnPromptPipeKey :: String
spawnPromptPipeKey = "DELVE_SPAWN_PROMPT"

promptResponsePipeKey :: String
promptResponsePipeKey = "DELVE_PROMPT_RESPONSE"

type Pipe = (Handle, Handle)

spawnCmd :: String -> CmdEnv -> CmdOutputHandler -> IO CmdInputHandler
spawnCmd cmd cmdEnv responseHandler = spawnCmd' cmd cmdEnv responseHandler

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
    appendFile "log" "closing descriptors\n"
    bitraverse_ hClose hClose pa
    bitraverse_ hClose hClose pb

pipeOutput :: (Handle, Handle) -> Handle
pipeOutput = fst

pipeInput :: (Handle, Handle) -> Handle
pipeInput = snd

spawnCmd' :: String -> CmdEnv -> CmdOutputHandler -> IO CmdInputHandler
spawnCmd' cmdStr cmdEnv responseHandler =
  do
    (spawnPromptPipe,promptResponsePipe) <- acquirePipeDescriptors
    let cmdInputHandler = hPutStr (pipeInput promptResponsePipe)
    void . forkIO
      $ do
        spawnPromptInFD <- show <$> handleToFd (pipeInput spawnPromptPipe)
        promptResponseOutFD <- show
          <$> handleToFd (pipeOutput promptResponsePipe)
        appendFile "log" ("spawning " ++ cmdStr ++ "\n")
        (_,_,_,procHandle) <- withFile "/dev/null" WriteMode
          $ \devNull -> do
            createProcess
              (proc cmdStr [])
                { env       = Just
                    $ M.toList cmdEnv
                    <> [ (spawnPromptPipeKey, spawnPromptInFD)
                       , (promptResponsePipeKey, promptResponseOutFD)
                       ]
                , std_in    = UseHandle devNull
                , std_out   = UseHandle devNull
                , std_err   = UseHandle devNull
                , close_fds = False
                }
        let promptWorker =
              promptWatcher spawnPromptPipe (responseHandler cmdInputHandler)
        appendFile "log" "making worker\n"
        void $ withAsync promptWorker $ const (waitForProcess procHandle)
        releasePipeDescriptors (spawnPromptPipe, promptResponsePipe)
    return cmdInputHandler

promptWatcher :: Pipe -> (String -> IO ()) -> IO ()
promptWatcher spawnPromptPipe responseHandler =
  do
    SP.effects
      (SP.mapM responseHandler $ SP.fromHandle (pipeOutput spawnPromptPipe))
