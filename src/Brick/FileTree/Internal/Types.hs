{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE OverloadedLists #-}
module Brick.FileTree.Internal.Types where

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

type SubTree = Cofree (GenericList String V.Vector) FileContext

data FileTree = FZ
  { parent :: S.Seq SubTree
  , context :: SubTree
  }

buildParent :: FilePath -> SubTree -> IO FileTree
buildParent p child = do
  FZ parents (c :< ls) <- newFileTree (takeDirectory p)
  let newChildren = fmap (replace p child) ls
  return $ FZ parents (c :< newChildren)
 where
  replace pth fc@((path -> pth') :< _) new | pth == pth' = new
                                           | otherwise   = fc

newFileTree :: FilePath -> IO FileTree
newFileTree currentDir = do
  absRoot        <- makeAbsolute (normalise currentDir)
  (_ FT.:/ tree) <- FT.buildL absRoot
  return $ convert (takeDirectory absRoot) tree

convert :: FilePath -> FT.DirTree FilePath -> FileTree
convert root tree = FZ [] . go (normalise root) $ tree
 where
  go :: FilePath -> FT.DirTree FilePath -> SubTree
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
