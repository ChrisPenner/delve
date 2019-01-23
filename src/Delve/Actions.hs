{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE PatternSynonyms #-}
module Delve.Actions where

import Brick.Widgets.Core
import Brick.Types
import Brick.Widgets.List
import qualified Data.Vector as V
import Data.Foldable
-- import Data.List.NonEmpty
-- import Data.Functor.Foldable
import Data.Functor.Compose
import Control.Comonad.Cofree as CF
import Data.List
import qualified System.Directory.Tree as FT

pattern UnC :: forall (f :: * -> *) (g :: * -> *) a.
  f (g (Cofree (Compose f g) a)) -> Cofree (Compose f g) a
pattern UnC x <- _ :< Compose x
{-# COMPLETE UnC #-}

data FileItem = Dir FilePath [FileItem] | File FilePath | Failed String
  deriving (Functor, Eq)

-- data FileZipper = FZ 
--   { parent :: [([FileItem], [FileItem])]
--   , current :: [FileItem]
--   }

data WidgetZipper = WZ
  { parent :: [(Int, List String FileItem)]
  , current :: List String FileItem
  }

-- display :: FSLayer -> String
-- display (UnC (File fp)) = "File " <> fp
-- display (UnC (Failed err)) = "Failed " <> err
-- display (UnC (Dir fp r)) = "Dir " <> fp <> "\n" <> (intercalate "\n" . toList . fmap (("  " ++) . displayLayer) $ r)

-- displayLayer :: FSLayer -> String
-- displayLayer (UnC (File fp)) = "File " <> fp
-- displayLayer (UnC (Failed err)) = "Failed " <> err
-- displayLayer (UnC (Dir fp _)) = "Dir " <> fp <> " "

-- findSelectedLayer :: FSLayer -> List _
-- findSelectedLayer ()


buildTree :: FilePath -> IO FSLayer
buildTree = fmap (convert . FT.dirTree) . crawlTree

crawlTree :: FilePath -> IO (FT.AnchoredDirTree FilePath)
crawlTree = FT.buildL

convert :: FT.DirTree FilePath -> FSLayer
convert = CF.unfold go
 where
  go
    :: (  FT.DirTree FilePath
       -> (Bool, Compose (GenericList String V.Vector) FileItem (FT.DirTree FilePath))
       )
  go (FT.Failed { FT.name, FT.err }) = (False, mempty)
  go (FT.File { FT.name }) = (False, mempty)
  go (FT.Dir path contents  ) = (False,  Compose . fromList . fmap (fmap embed) $ (path, contents))

embed :: FT.DirTree FilePath -> FileItem FilePath
embed = undefined

fromList :: (String, [r]) ->  List String (r)
fromList (p, ls) = list p (V.fromList ls) 1

renderFSLayer :: FSLayer -> Widget String
renderFSLayer (UnC (Dir _ xs)) = renderList (const renderSingle) True xs 
renderFSLayer f = renderSingle f

renderSingle :: FSLayer -> Widget String
renderSingle (UnC (File fp)) = str fp
renderSingle (UnC (Failed err)) = str ("! " <> err)
renderSingle (UnC (Dir p _)) = str (p <> "/")
