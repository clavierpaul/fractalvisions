{-# LANGUAGE CPP                 #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Processing where

import Codec.Picture
import Data.Colour.RGBSpace.HSV
import Data.Colour.RGBSpace
import GHC.Word
import Data.List

import Control.Monad.ST
import qualified Codec.Picture.Types as M
  
hsvToRGB :: Double -> Double -> Double -> PixelRGB8
hsvToRGB h s v = PixelRGB8 r g b
  where
    hh = h / 60.0
    i = fromIntegral $ floor hh
    ff = hh - i
    p = fromIntegral $ round $ 255 * v * (1.0 - s)
    q = fromIntegral $ round $ 255 * v * (1.0 - (s * ff))
    t = fromIntegral $ round $ 255 * v * (1.0 - (s * (1.0 - ff)))
    nV = fromIntegral $ round $ 255 * v
    (r, g, b) = case i of
      0.0 -> (nV,  t,  p)
      1.0 -> ( q, nV,  p)
      2.0 -> ( p, nV,  t)
      3.0 -> ( p,  q, nV)
      4.0 -> ( t,  p, nV)
      _   -> (nV,  p,  q)
