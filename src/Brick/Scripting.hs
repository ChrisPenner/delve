module Brick.Scripting where

import System.IO
import Control.Exception
import System.Process
import Control.Concurrent.Async
import Brick.BChan
import Brick.Types
import Brick.Widgets.Dialog
import Brick.Widgets.FileTree
import Delve.Events
import qualified Streaming.Prelude as SP
import Control.Monad
import Control.Monad.IO.Class
import GHC.IO.Handle.FD
import Data.Bifoldable
import Control.Concurrent
import Data.Maybe
import System.Posix.Process

data ScriptingData =
  SD { dialog :: Maybe (Dialog String)
     }

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

handleCmd :: String -> FileTree a -> EventM String ()
handleCmd cmd ft = void . liftIO . forkIO $ bracket acquirePipeDescriptors
                                                    (spawnCmd cmd)
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

spawnCmd :: String -> (Pipe, Pipe) -> IO ()
spawnCmd cmdStr (spawnDialogP, dialogResponseP) = do
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
  let dialogWorker = dialogWatcher spawnDialogP undefined
  void $ withAsync dialogWorker $ const (waitForProcess procHandle)

dialogWatcher :: Pipe -> BChan DelveEvent -> IO ()
dialogWatcher dialogRequestP eChan = do
  SP.effects
    ( SP.mapM (sendToChan . ScriptEvent . SpawnDialog)
    $ SP.fromHandle (pipeOutput dialogRequestP)
    )
 where
  sendToChan :: DelveEvent -> IO ()
  sendToChan de = writeBChan eChan de

openInVim :: FileTree a -> EventM r ()
openInVim ft = do
  let f = getCurrentFilePath ft
  liftIO $ executeFile "vim" True (maybeToList f) Nothing
