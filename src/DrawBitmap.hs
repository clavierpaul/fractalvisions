module DrawBitmap where

import Circle
import Bitmap
import Control.Monad.ST
import Codec.Picture
import Control.Parallel.Strategies
import Text.Printf
import Data.Array

-- DEBUGGING
-- generate a PNG of a circle with the given parameters
testCircle :: Int -> Int -> Int -> IO()
testCircle frameSize radius thickness = writePng "test/TEST.png" $ generateImage pixelRenderer frameSize frameSize
  where
    centre = (frameSize `div` 2,frameSize `div` 2)
    circlePoints = concatMap (generateCirclePoints centre) [(radius-(thickness `div` 2))..(radius + (thickness `div` 2))]
    pixelRenderer x y | (x,y) `elem` circlePoints   = PixelRGB8 0   0   0
                      | otherwise                   = PixelRGB8 255 255 255


genCirclePoints :: Int -> Int -> Int -> Int -> [Bool]
genCirclePoints w h radius thickness = pixels
  where
    circlePoints = concatMap (generateCirclePoints centre) [(radius-(thickness `div` 2))..(radius + (thickness `div` 2))]
    centre = (w `div` 2,h `div` 2)
    bounds = ((0,0),(w-1,h-1))
    pixels = map (`elem` circlePoints) (range bounds)
    