module Processing where

import Codec.Picture
import Data.Colour.RGBSpace.HSV
import Data.Colour.RGBSpace
import GHC.Word
import Data.List

hsvToRGB :: Int -> Double -> Double -> PixelRGB8
hsvToRGB h s v = PixelRGB8 r g b
    where 
        c = v*s
        x = c*fromIntegral (1 - abs ( (h `div` 60) `mod` 2 - 1) )
        m = v-c
        scaledRGBs
            |   0 <= h && h <  60 = [c,x,0]
            |  60 <= h && h < 120 = [x,c,0]
            | 120 <= h && h < 180 = [0,c,x]
            | 180 <= h && h < 240 = [0,x,c]
            | 240 <= h && h < 300 = [x,0,c]
            | otherwise           = [c,0,x]
        (r:g:b:[]) = map (\x -> fromInteger $ round $ (x+m)*255) scaledRGBs


saturation :: Double -> PixelRGB8 -> PixelRGB8
saturation s (PixelRGB8 r g b) = hsvToRGB h s v
    where
        scaledRGBs = map (\x -> (fromIntegral x) / 255.0) [r, g, b] :: [Double]
        (r':g':b':[]) = scaledRGBs
        cMax = maximum scaledRGBs
        cMin = minimum scaledRGBs
        delta = cMax - cMin
        h | cMax == r' = 60 * (round ((g'-b')/delta) `mod` 6)
          | cMax == g' = 60 * (round ((b'-r')/delta) + 2)
          | cMax == b' = 60 * (round ((r'-g')/delta) + 4)
          | otherwise  = 0
        v = cMax


-- Legacy code by Advaith with s removed as this is already provided and thus does not need to be calculated:

--       cMax = maximum [r',g',b']
--       cMin = minimum [r',g',b']
--       delta = cMax - cMin
--       h | cMax == r = 
--             if (g-b) <= 0 
--                 then fromIntegral (360 + toInteger (60 * ((((g - b) `div` delta))))) 
--                 else fromIntegral (60 * ((((g - b) `div` delta))))
--         | cMax == g = if (b-r) <= 0 then fromIntegral (360 + toInteger (2 + (60 * ((((b - r) `div` delta)))))) else fromIntegral (60 * ((((b - r) `div` delta))))
--         | cMax == b = if (r-g) <= 0 then fromIntegral (360 + toInteger (4 + (60 * ((((r - g) `div` delta)))))) else fromIntegral (60 * ((((r - g) `div` delta))))
--         | otherwise = fromIntegral $ toInteger (-1)
--       v | cMax == 0 = undefined
--         | otherwise = cMax
-- we know that r,g,b are values between [0..1]
-- h = 0,360 s = [0,1] v = [0,1]