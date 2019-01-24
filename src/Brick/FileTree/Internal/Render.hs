{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ViewPatterns #-}
module Brick.FileTree.Internal.Render where

import Brick.FileTree.Internal.Types

import Data.Foldable
import Brick.Widgets.Core
import Brick.Widgets.Border
import Brick.Types
import Brick.Widgets.List
import Control.Comonad.Cofree as CF
import Control.Comonad
import qualified Data.Sequence as S
import Data.Bool

cacheKey :: FileContext -> String
cacheKey = path

renderHeader :: SubTree -> Widget String
renderHeader ((path -> p) :< _) = str p <=> hBorder

renderFileTree :: FileTree -> Widget String
renderFileTree (FZ { parents, context }) =
  renderHeader context <=> (renderParents parents <+> renderNode context)

renderSelection :: FileTree -> Widget String
renderSelection (FZ { selection }) = vBox . fmap str . toList $ selection

renderParents :: S.Seq SubTree -> Widget String
renderParents S.Empty                    = emptyWidget
renderParents parents@(_ S.:|> (p :< _)) = cached
  (cacheKey p)
  (hBox . toList $ (renderParent <$> S.drop ind parents))
 where
  len = S.length parents
  ind = max 0 (len - 2)

renderNode :: SubTree -> Widget String
renderNode (_ :< ls) = renderList (const (renderFileContext . extract)) True ls

renderParent :: SubTree -> Widget String
renderParent = (<+> vBorder) . hLimit 20 . renderNode

renderFileContext :: FileContext -> Widget String
renderFileContext (FC { kind = File, name, selected }) =
  str $ (bool "" "* " selected) <> name
renderFileContext (FC { kind = Error, name, path }) =
  str ("! " <> path <> ": " <> name)
renderFileContext (FC { kind = Dir, name, selected }) =
  str $ (bool "" "* " selected) <> name <> "/"
