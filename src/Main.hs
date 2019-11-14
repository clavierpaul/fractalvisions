{-# LANGUAGE RankNTypes #-}

module Main where

import Section

main :: IO ()
main = do
  iA <- introA 
  iB <- introB iA
  v1 <- verse1 iB
  b1a <- bridge1A v1
  return ()
