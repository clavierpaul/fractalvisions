{-# LANGUAGE RankNTypes #-}

module Main where

import Section

main :: IO ()
main = do
  iA <- introA 
  iB <- introB iA
  verseA iB
  return ()
