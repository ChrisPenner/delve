{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE OverloadedLists #-}
module Brick.FileTree.Actions where

import Brick.Main
import Brick.Types
import Brick.Widgets.List
import Control.Comonad.Cofree as CF
import qualified Data.Sequence as S
import Brick.FileTree.Types
import Brick.FileTree.Render
import Control.Monad.IO.Class

ascendDir :: FileTree -> EventM String FileTree
ascendDir (FZ S.Empty tree@((path -> p):<_)) = do
  liftIO $ buildParent p tree
ascendDir (FZ (ps S.:|> (f :< pList)) current) = do
  invalidateCacheEntry (cacheKey f)
  return $ FZ ps (f :< listModify (const current) pList)

descendDir :: FileTree -> EventM String FileTree
descendDir fz@(FZ parents (f:< children)) = do
  invalidateCacheEntry (cacheKey f)
  return $ case listSelectedElement children of
    Nothing -> fz
    Just (_, nextChildren@(FC{kind=Dir} :< _)) -> FZ (parents S.|> (f:<children)) nextChildren
    Just _ -> fz

select :: FileTree -> Maybe FilePath
select (FZ _ (_:< children)) = 
  case listSelectedElement children of
    Nothing -> Nothing
    Just (_, FC{kind=Error} :< _) -> Nothing
    Just (_, fc:< _) -> Just (path fc)
