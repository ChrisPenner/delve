{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE OverloadedLists #-}
module Delve.Actions where

import Data.Foldable
import Brick.Widgets.Core
import Brick.Widgets.Border
import Brick.Types
import Brick.Widgets.List
import qualified Data.Vector as V
import Control.Comonad.Cofree as CF
import qualified System.Directory.Tree as FT
import qualified Data.Sequence as S

data FileItem = Dir FilePath String | File FilePath String | Failed FilePath String
  deriving (Eq, Show)

type FileTree = Cofree (GenericList String V.Vector) FileItem

data FileZipper = FZ
  { parent :: S.Seq FileTree
  , context :: FileTree
  }

buildTree :: FilePath -> IO FileZipper
buildTree = fmap (convert . FT.dirTree) . crawlTree

crawlTree :: FilePath -> IO (FT.AnchoredDirTree FilePath)
crawlTree = FT.build

convert :: FT.DirTree FilePath -> FileZipper
convert = FZ [] . go
 where
  go :: FT.DirTree FilePath -> FileTree
  go (FT.Failed { FT.name, FT.err }) = Failed name (show err) :< list name mempty 1
  go (FT.File { FT.name, FT.file }) = File file name :< list name mempty 1
  go (FT.Dir path contents  ) = Dir path path :< list path (V.fromList . fmap go $ contents) 1

renderFileZipper :: FileZipper -> Widget String
renderFileZipper (FZ parents (_ :< lst)) = (hBox . toList $ (renderParent <$> S.drop ind parents)) <+> renderList (const renderFileItem) True lst
  where
    len = S.length parents
    ind = max 0 (len - 2)

renderParent :: FileTree -> Widget String
renderParent (_ :< ls) = 
  let w = renderList (const renderFileItem) True ls
   in hLimit 20 w <+> vBorder

renderFileItem :: FileTree -> Widget String
renderFileItem (File _ name :< _) = str name
renderFileItem (Failed path err :< _) = str ("! " <> path <> ": " <> err)
renderFileItem (Dir _ p :< _) = str (p <> "/")

ascendDir :: FileZipper -> FileZipper
ascendDir fz@(FZ S.Empty _) = fz
ascendDir (FZ (ps S.:|> (f :< pList)) current) = FZ ps (f :< listModify (const current) pList)

descendDir :: FileZipper -> FileZipper
descendDir fz@(FZ parents (f:< children)) =
  case listSelectedElement children of
    Nothing -> fz
    Just (_, nextChildren@(Dir{} :< _)) -> FZ (parents S.|> (f:<children)) nextChildren
    Just _ -> fz

select :: FileZipper -> Maybe FilePath
select (FZ _ (_:< children)) = 
  case listSelectedElement children of
    Nothing -> Nothing
    Just (_, File path _:<_) -> Just path
    Just (_, Dir path _:<_) -> Just path
    Just (_, Failed {} :<_) -> Nothing
