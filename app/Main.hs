module Main
  ( main
  )
where

import           Brick

import           Delve
import           Brick.Widgets.FileTree

main :: IO ()
main = do
  fb  <- newFileTree "."
  res <- defaultMain app fb
  putStrLn $ getCurrentDir res
