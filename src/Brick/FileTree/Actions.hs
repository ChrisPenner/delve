{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE OverloadedLists #-}
module Brick.FileTree.Actions where

import qualified Graphics.Vty.Input as V
import Brick.Main
import Brick.Types
import Brick.Widgets.List
import Control.Comonad.Cofree as CF
import qualified Data.Sequence as S
import Brick.FileTree.Types
import Brick.FileTree.Render
import Control.Monad.IO.Class

overCurrentList :: (List String SubTree -> EventM String (List String SubTree)) -> FileTree -> EventM String FileTree
overCurrentList f fz@(FZ _ (x:<lst)) = do
  newLst <- f lst
  return fz{context=x:<newLst}

pressKey :: V.Key -> (FileTree -> EventM String FileTree)
pressKey k = overCurrentList (handleListEvent (V.EvKey k []) )

moveDown :: FileTree -> EventM String FileTree
moveDown = pressKey V.KDown

moveUp :: FileTree -> EventM String FileTree
moveUp = pressKey V.KUp

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

getSelectedFilePath :: FileTree -> Maybe FilePath
getSelectedFilePath (FZ _ (_:< children)) = 
  case listSelectedElement children of
    Nothing -> Nothing
    Just (_, FC{kind=Error} :< _) -> Nothing
    Just (_, fc:< _) -> Just (path fc)
