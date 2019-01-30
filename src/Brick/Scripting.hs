{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedStrings #-}
module Brick.Scripting where

import Brick.Widgets.Core
import Brick.BChan
import Brick.Types
import Brick.Widgets.Edit
import Brick.Widgets.FileTree
import Brick.Widgets.Border
import Brick.AttrMap
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
import GHC.Generics
import GHC.IO.Handle.FD
import System.IO
import System.Posix.Process
import System.Process
import qualified Streaming.Prelude as SP

newtype ScriptingData =
  SD { prompt :: Maybe (Editor String String)
     } deriving Generic

_prompt :: Lens' ScriptingData (Maybe (Editor String String))
_prompt = field @"prompt"

flaggedKey :: String
flaggedKey = "DELVE_FLAGGED"

focusedKey :: String
focusedKey = "DELVE_FOCUSED"

currentDirKey :: String
currentDirKey = "DELVE_CURRENT_DIR"

spawnDialogPipeKey :: String
spawnDialogPipeKey = "DELVE_SPAWN_DIALOG"

dialogResponsePipeKey :: String
dialogResponsePipeKey = "DELVE_DIALOG_RESPONSE"

type Pipe = (Handle, Handle)

handleCmd :: BChan DelveEvent -> String -> EventM String ()
handleCmd eChan cmd = void . liftIO . forkIO $ bracket acquirePipeDescriptors
                                                       (spawnCmd eChan cmd)
                                                       releasePipeDescriptors

-- simpleCommand :: FileTree a -> String -> EventM String ()
-- simpleCommand ft cmd = liftIO $ do
--   let currentFilePath = fromMaybe "" $ getCurrentFilePath ft
--   let currentDir      = getCurrentDir ft
--   let flaggedItems    = getFlagged ft
--   withFile "/dev/null" WriteMode
--     $ \devNull -> do
--       (_, _, _, pHandle) <- createProcess (proc cmd [])
--         { env     = Just
--           [ (flaggedKey   , intercalate "\n" flaggedItems)
--           , (focusedKey   , currentFilePath)
--           , (currentDirKey, currentDir)
--           ]
--         , std_in  = UseHandle devNull
--         , std_out = UseHandle devNull
--         , std_err = UseHandle devNull
--         }



acquirePipeDescriptors :: IO (Pipe, Pipe)
acquirePipeDescriptors = do
  spawnDialogPipe    <- createPipe
  dialogResponsePipe <- createPipe
  return (spawnDialogPipe, dialogResponsePipe)

releasePipeDescriptors :: (Pipe, Pipe) -> IO ()
releasePipeDescriptors (pa, pb) = do
  bitraverse_ hClose hClose pa
  bitraverse_ hClose hClose pb

pipeInput :: (a, b) -> a
pipeInput = fst
pipeOutput :: (a, b) -> b
pipeOutput = snd

spawnCmd :: BChan DelveEvent -> String -> (Pipe, Pipe) -> IO ()
spawnCmd eChan cmdStr (spawnDialogP, dialogResponseP) = do
  spawnDialogInFD       <- show <$> handleToFd (pipeInput spawnDialogP)
  dialogResponseOutFD   <- show <$> handleToFd (pipeOutput dialogResponseP)
  (_, _, _, procHandle) <- withFile "/dev/null" WriteMode $ \devNull -> do
    createProcess (proc cmdStr [])
      { env     = Just
        [ (spawnDialogPipeKey   , spawnDialogInFD)
        , (dialogResponsePipeKey, dialogResponseOutFD)
        ]
      , std_in  = UseHandle devNull
      , std_out = UseHandle devNull
      , std_err = UseHandle devNull
      }
  let dialogWorker = (dialogWatcher cmdStr) spawnDialogP eChan
  void $ withAsync dialogWorker $ const (waitForProcess procHandle)

dialogWatcher :: String -> Pipe -> BChan DelveEvent -> IO ()
dialogWatcher cmdStr dialogRequestP eChan = do
  SP.effects
    ( SP.mapM (sendToChan . ScriptEvent . SpawnDialog cmdStr)
    $ SP.fromHandle (pipeOutput dialogRequestP)
    )
 where
  sendToChan :: DelveEvent -> IO ()
  sendToChan de = writeBChan eChan de

openInVim :: FileTree a -> EventM r ()
openInVim ft = do
  let f = getCurrentFilePath ft
  liftIO $ executeFile "vim" True (maybeToList f) Nothing

handleScriptEvents :: ScriptEvent -> ScriptingData -> EventM r ScriptingData
handleScriptEvents (SpawnDialog cmd q) s =
  let numLines = Just 1
      eName    = cmd
  in  return $ (s & _prompt ?~ editor eName numLines q)

scriptOverlayBGAttr :: AttrName
scriptOverlayBGAttr = "script-overlay-bg"

renderScripting :: ScriptingData -> Widget String
renderScripting SD { prompt = Just e } =
  withAttr scriptOverlayBGAttr
    $   padTopBottom 2
    $   renderEditor (vBox . fmap str) True e
    <=> hBorder
renderScripting _ = emptyWidget
