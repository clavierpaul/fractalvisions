{-# LANGUAGE RankNTypes #-}

module Main where

import Section

main :: IO ()
main = do
  iA <- introA 
  iB <- introB iA
  v1 <- verse1 iB
  b1a <- bridge1A v1
  b1b <- bridge1B b1a
  b1c <- bridge1C b1b
  return ()
