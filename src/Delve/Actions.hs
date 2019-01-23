{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE PatternSynonyms #-}
module Delve.Actions where

import Brick.Widgets.Core
import Brick.Types
import Brick.Widgets.List hiding (reverse)
import qualified Data.Vector as V
import Data.Functor.Compose
import Control.Comonad.Cofree as CF
import qualified System.Directory.Tree as FT

pattern UnC :: forall (f :: * -> *) (g :: * -> *) a.
  f (g (Cofree (Compose f g) a)) -> Cofree (Compose f g) a
pattern UnC x <- _ :< Compose x
{-# COMPLETE UnC #-}

data FileItem = Dir FilePath | File FilePath | Failed FilePath String
  deriving (Eq, Show)

type FileTree = Cofree (GenericList String V.Vector) FileItem

data FileZipper = FZ
  { parent :: [(Int, FileTree)]
  , context :: FileTree
  }

buildTree :: FilePath -> IO FileZipper
buildTree = fmap (convert . FT.dirTree) . crawlTree

crawlTree :: FilePath -> IO (FT.AnchoredDirTree FilePath)
crawlTree = FT.buildL

convert :: FT.DirTree FilePath -> FileZipper
convert = FZ [] . go
 where
  go :: FT.DirTree FilePath -> FileTree
  go (FT.Failed { FT.name, FT.err }) = (Failed name (show err) :< list name mempty 1)
  go (FT.File { FT.name, FT.file }) = (File file :< list name mempty 1)
  go (FT.Dir path contents  ) = (Dir path :< list path (V.fromList . fmap go $ contents) 1)

renderFileZipper :: FileZipper -> Widget String
renderFileZipper (FZ parents (_ :< lst)) = hBox (renderParent <$> reverse parents) <+> renderList (const renderFileItem) True lst

renderParent :: (Int, FileTree) -> Widget String
renderParent (_, _ :< ls) = 
  let w = renderList (const renderFileItem) True ls
   in hLimit 20 w

renderFileItem :: FileTree -> Widget String
renderFileItem (File fp :< _) = str fp
renderFileItem (Failed path err :< _) = str ("! " <> path <> ": " <> err)
renderFileItem (Dir p :< _) = str (p <> "/")

ascendDir :: FileZipper -> FileZipper
ascendDir fz@(FZ [] _) = fz
ascendDir (FZ ((i, f :< pList):ps) current) = FZ ps (f :< pList)

descendDir :: FileZipper -> FileZipper
descendDir fz@(FZ parents (f:< children)) =
  case listSelectedElement children of
    Nothing -> fz
    Just (i, nextChildren) -> FZ ((i, f:<children):parents) nextChildren
