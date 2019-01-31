{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}
module Brick.Scripting where

import Brick.AttrMap
import Brick.BChan
import Brick.Types
import Brick.Widgets.Border
import Brick.Widgets.Core
import Brick.Widgets.Edit
import Brick.Widgets.FileTree
import Control.Arrow ((&&&))
import Control.Concurrent
import Control.Concurrent.Async
import Control.Exception
import Control.Lens
import Control.Monad
import Control.Monad.IO.Class
import Data.Bifoldable
import Data.Generics.Product
import Data.Maybe
import Delve.Events
import GHC.Generics (Generic)
import GHC.IO.Handle.FD
import System.IO
import System.Posix.Process
import System.Process
import GHC.IO.Handle
import qualified Graphics.Vty.Input.Events as V
import qualified Streaming.Prelude as SP

data ScriptingData =
  SD { promptData :: Maybe PromptData
     } deriving Generic

data PromptData = PD
  { prompt :: Editor String String
  , responseHandle :: Handle
  } deriving Generic

_promptData :: Lens' ScriptingData (Maybe PromptData)
_promptData = field @"promptData"

_prompt :: Lens' PromptData (Editor String String)
_prompt = field @"prompt"

_responseHandle :: Lens' PromptData Handle
_responseHandle = field @"responseHandle"


flaggedKey :: String
flaggedKey = "DELVE_FLAGGED"

focusedKey :: String
focusedKey = "DELVE_FOCUSED"

currentDirKey :: String
currentDirKey = "DELVE_CURRENT_DIR"

spawnPromptPipeKey :: String
spawnPromptPipeKey = "DELVE_SPAWN_PROMPT"

promptResponsePipeKey :: String
promptResponsePipeKey = "DELVE_PROMPT_RESPONSE"

type Pipe = (Handle, Handle)

handleCmd :: BChan DelveEvent -> String -> EventM String ()
handleCmd eChan cmd = void . liftIO . forkIO $ bracket acquirePipeDescriptors

                                                       releasePipeDescriptors
                                                       (spawnCmd eChan cmd)

acquirePipeDescriptors :: IO (Pipe, Pipe)
acquirePipeDescriptors = do
  spawnPromptPipe    <- createPipe
  promptResponsePipe <- createPipe
  return (spawnPromptPipe, promptResponsePipe)

releasePipeDescriptors :: (Pipe, Pipe) -> IO ()
releasePipeDescriptors (pa, pb) = do
  bitraverse_ hClose hClose pa
  -- bitraverse_ hClose hClose pb

pipeOutput :: (Handle, Handle) -> Handle
pipeOutput = fst
pipeInput :: (Handle, Handle) -> Handle
pipeInput = snd

spawnCmd :: BChan DelveEvent -> String -> (Pipe, Pipe) -> IO ()
spawnCmd eChan cmdStr (spawnPromptPipe, promptResponsePipe) = do
  spawnPromptInFD       <- show <$> handleToFd (pipeInput spawnPromptPipe)
  promptResponseOutFD   <- show <$> handleToFd (pipeOutput promptResponsePipe)
  (_, _, _, procHandle) <- withFile "/dev/null" WriteMode $ \devNull -> do
    createProcess (proc cmdStr [])
      { env       = Just
        [ (spawnPromptPipeKey   , spawnPromptInFD)
        , (promptResponsePipeKey, promptResponseOutFD)
        ]
      , std_in    = UseHandle devNull
      , std_out   = UseHandle devNull
      , std_err   = UseHandle devNull
      , close_fds = False
      }
  let promptWorker =
        promptWatcher cmdStr spawnPromptPipe promptResponsePipe eChan
  void $ withAsync promptWorker $ const (waitForProcess procHandle)

promptWatcher :: String -> Pipe -> Pipe -> BChan DelveEvent -> IO ()
promptWatcher cmdStr spawnPromptPipe promptResponsePipe eChan = do
  SP.effects
    ( SP.mapM
        (sendToChan . ScriptEvent . SpawnPrompt cmdStr
                                                (pipeInput promptResponsePipe)
        )
    $ SP.fromHandle (pipeOutput spawnPromptPipe)
    )
 where
  sendToChan :: DelveEvent -> IO ()
  sendToChan de = writeBChan eChan de

openInVim :: FileTree a -> EventM r ()
openInVim ft = do
  let f = getCurrentFilePath ft
  liftIO $ executeFile "vim" True (maybeToList f) Nothing

handleScriptEvents :: ScriptEvent -> ScriptingData -> EventM r ScriptingData
handleScriptEvents (SpawnPrompt cmd promptResponseH q) s =
  let numLines = Just 1
      eName    = cmd
  in  return
        (s
          { promptData = Just $ PD
            { prompt         = editor eName numLines q
            , responseHandle = promptResponseH
            }
          }
        )


handleScriptingEvent :: V.Event -> ScriptingData -> EventM String ScriptingData
handleScriptingEvent (V.EvKey V.KEnter _) s = do
  case
      s ^? _promptData . _Just . to
        (getEditContents . prompt &&& responseHandle)
    of
      Nothing            -> return ()
      Just (txtLines, h) -> liftIO $ do
        liftIO . hPutStrLn h $ unlines txtLines
        liftIO $ hClose h
  return (s & _promptData .~ Nothing)
handleScriptingEvent e s =
  (_promptData . _Just . _prompt %%~ handleEditorEvent e) s

scriptOverlayBGAttr :: AttrName
scriptOverlayBGAttr = "script-overlay-bg"

renderPrompt :: Editor String String -> Widget String
renderPrompt p =
  withAttr scriptOverlayBGAttr
    $   padTopBottom 2
    $   renderEditor (vBox . fmap str) True p
    <=> hBorder

renderScripting :: ScriptingData -> Widget String
renderScripting sd =
  fromMaybe emptyWidget $ sd ^? _promptData . _Just . _prompt . to renderPrompt
