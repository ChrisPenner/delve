{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ViewPatterns #-}
module Delve.Render where

import Delve.Types

import Data.Foldable
import Brick.Widgets.Core
import Brick.Widgets.Border
import Brick.Types
import Brick.Widgets.List
import Control.Comonad.Cofree as CF
import Control.Comonad
import qualified Data.Sequence as S



cacheKey :: FileContext -> String
cacheKey = path

renderHeader :: FileTree -> Widget String
renderHeader ((path -> p) :< _) = str p <=> hBorder

renderFileZipper :: FileZipper -> Widget String
renderFileZipper (FZ ps node) =
  renderHeader node <=> (renderParents ps <+> renderNode node)

renderParents :: S.Seq FileTree -> Widget String
renderParents S.Empty                    = emptyWidget
renderParents parents@(_ S.:|> (p :< _)) = cached
  (cacheKey p)
  (hBox . toList $ (renderParent <$> S.drop ind parents))
 where
  len = S.length parents
  ind = max 0 (len - 2)

renderNode :: FileTree -> Widget String
renderNode (_ :< ls) = renderList (const (renderFileItem . extract)) True ls

renderParent :: FileTree -> Widget String
renderParent = (<+> vBorder) . hLimit 20 . renderNode

renderFileItem :: FileContext -> Widget String
renderFileItem (FC { kind = File, name }) = str name
renderFileItem (FC { kind = Error, name, path }) =
  str ("! " <> path <> ": " <> name)
renderFileItem (FC { kind = Dir, name }) = str (name <> "/")
