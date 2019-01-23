{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedLists #-}
module Delve.Types where

import Brick.Widgets.List
import qualified Data.Vector as V
import Control.Comonad.Cofree as CF
import qualified System.Directory.Tree as FT
import qualified Data.Sequence as S
import System.FilePath.Posix
import System.Directory

data FileKind = Dir | File | Error

data FileContext =
  FC
    { selected :: Bool
    , path :: FilePath
    , name :: String
    , kind :: FileKind
    }

type FileTree = Cofree (GenericList String V.Vector) FileContext

data FileZipper = FZ
  { parent :: S.Seq FileTree
  , context :: FileTree
  }

buildTree :: FilePath -> IO FileZipper
buildTree currentDir = do
  (root FT.:/ tree) <- crawlTree currentDir
  absRoot           <- makeAbsolute root
  return $ convert absRoot tree

crawlTree :: FilePath -> IO (FT.AnchoredDirTree FilePath)
crawlTree = FT.build

convert :: FilePath -> FT.DirTree FilePath -> FileZipper
convert root tree = FZ [] . go root $ tree
 where
  go :: FilePath -> FT.DirTree FilePath -> FileTree
  go root' (FT.Failed { FT.name, FT.err }) =
    FC
        { name     = show err
        , path     = normalise (root' </> name)
        , selected = False
        , kind     = Error
        }
      :< list name mempty 1
  go root' (FT.File { FT.name }) =
    FC
        { name     = name
        , path     = normalise (root' </> name)
        , selected = False
        , kind     = File
        }
      :< list name mempty 1
  go root' (FT.Dir path contents) =
    FC
        { name     = path
        , path     = normalise (root' </> path)
        , kind     = Dir
        , selected = False
        }
      :< list path (V.fromList . fmap (go (root' </> path)) $ contents) 1
