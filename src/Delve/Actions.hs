{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE OverloadedLists #-}
module Delve.Actions where

import Brick.Main
import Brick.Types
import Brick.Widgets.List
import Control.Comonad.Cofree as CF
import qualified Data.Sequence as S
import Delve.Types
import Delve.Render

ascendDir :: FileZipper -> EventM String FileZipper
ascendDir fz@(FZ S.Empty _) = pure fz
ascendDir (FZ (ps S.:|> (f :< pList)) current) = do
  invalidateCacheEntry (cacheKey f)
  return $ FZ ps (f :< listModify (const current) pList)

descendDir :: FileZipper -> EventM String FileZipper
descendDir fz@(FZ parents (f:< children)) = do
  invalidateCacheEntry (cacheKey f)
  return $ case listSelectedElement children of
    Nothing -> fz
    Just (_, nextChildren@(FC{kind=Dir} :< _)) -> FZ (parents S.|> (f:<children)) nextChildren
    Just _ -> fz

select :: FileZipper -> Maybe FilePath
select (FZ _ (_:< children)) = 
  case listSelectedElement children of
    Nothing -> Nothing
    Just (_, FC{kind=Error} :< _) -> Nothing
    Just (_, fc:< _) -> Just (path fc)
