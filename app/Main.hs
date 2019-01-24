module Main
  ( main
  )
where

import           Brick

import           Delve
import           Brick.Widgets.FileTree

main :: IO ()
main = do
  fb  <- newFileTree (const pure) "."
  res <- defaultMain app fb
  putStrLn $ getCurrentDir res
