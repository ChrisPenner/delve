module Brick.Scripting where

import Brick.Types
import System.IO
import System.Process hiding (createPipe)
import System.Environment
import System.Posix.IO
import System.Posix.Types
import Control.Exception
import Foreign.C.Types
import Data.Foldable
import Control.Monad.IO.Class
import Control.Concurrent.Async

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

type Pipe = (Fd, Fd)

acquirePipeDescriptors :: IO (Pipe, Pipe)
acquirePipeDescriptors = do
  spawnDialogPipe    <- createPipe
  dialogResponsePipe <- createPipe
  return (spawnDialogPipe, dialogResponsePipe)

releasePipeDescriptors :: (Pipe, Pipe) -> IO ()
releasePipeDescriptors ((a, b), (c, d)) = traverse_ closeFd [a, b, c, d]

runCommand :: EventM r ()
runCommand =
  liftIO $ bracket acquirePipeDescriptors releasePipeDescriptors runCommand'

runCommand' :: (Pipe, Pipe) -> IO ()
runCommand' ((spawnDialogIn, spawnDialogOut), (dialogResponseIn, dialogResponseOut))
  = do
    (_, _, _, pHandle) <- createProcess (proc "scr" [])
      { env = Just
        [ (spawnDialogPipeKey   , show spawnDialogIn)
        , (dialogResponsePipeKey, show dialogResponseOut)
        ]
      }
    spawnDialogOutH <- fdToHandle spawnDialogOut
    pWait           <- async (waitForProcess pHandle)
    dialogRequest   <- async (hGetLine spawnDialogOutH)
    result          <- waitEither pWait dialogRequest
    case result of
      Left  _        -> return ()
      Right dRequest -> undefined
