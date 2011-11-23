{-# OPTIONS -Wall -Werror -fwarn-tabs -fno-warn-type-defaults -fno-warn-orphans -fno-warn-unused-do-bind #-}

module Main (main) where

import PoemClassifier

main :: IO ()
main = do
  -- Read the poem in from STDIN
  poem <- getContents
  -- echo the poem and its type
  putStrLn $ "Poem: \n" ++ poem
  putStrLn $ "Type: " ++ classify poem


