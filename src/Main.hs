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
  -- b1c <- bridge1C b1b
  -- c1 <- chorus1 b1c
  -- ce <- chorusend c1
  -- b2 <- bridge2 ce
  -- v2a <- verse2a b2
  -- v2b <- verse2b v2a
  -- v2c <- verse2c v2b
  -- pc <- prechorus v2c
  -- c2 <- chorus2 pc
  -- outro c2
  return ()
