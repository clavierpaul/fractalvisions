module Fractal (
    Julia(Julia),
    height,
    width,
    zoom,
    cX,
    cY,
    maxIter,
    fractalLoop,
    genFractal
) where

import Data.Bits
import Codec.Picture
import Processing

data Julia = 
     Julia { height :: Int 
           , width :: Int
           , zoom :: Double
           , cX :: Double
           , cY :: Double 
           , maxIter :: Int
           , frame :: Int
           , sat :: Double
           } 
           deriving (Eq,Show,Ord)

fractalLoop :: Double -> Double -> Double -> Double -> Int -> Double
fractalLoop zx zy cX cY maxIter
  | zx**2 + zy**2 >= 4 || maxIter <= 1 = fromIntegral  (maxIter + 1 - round ((log (sqrt (zx ** 2 + zy ** 2))) / log 2))
  | otherwise = fractalLoop tmp czy cX cY (maxIter - 1)
    where tmp = zx**2 - zy**2 + cX
          czy = 2*zx*zy + cY

genFractal :: Julia -> Int -> Int -> PixelRGB8
genFractal fract x y = pixel
  where 
    h  = fromIntegral $ height fract
    w  = fromIntegral $ width fract
    z  = zoom fract 
    f  = frame fract
    s  = sat fract
    cx = cX fract 
    cy = cY fract
    dx = fromIntegral x
    dy = fromIntegral y
    zx = 1.5*(dx-w/2)/(0.5*z*w)
    zy = (dy-h/2)/(0.5*z*h)

    i = fractalLoop zx zy cx cy $ maxIter fract
    v = abs $ sin i
    
    pixel = hsvToRGB (fromIntegral $ f `mod` 360) s v