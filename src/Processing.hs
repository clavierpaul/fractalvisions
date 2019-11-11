module Processing where

import Codec.Picture
import Data.Colour.RGBSpace.HSV
import Data.Colour.RGBSpace
import GHC.Word
import Data.List

saturation :: Double -> PixelRGB8 -> PixelRGB8 
saturation s (PixelRGB8 r g b) = convert (PixelRGB8 r g b)
    where 
--        cR = fromIntegral r
--        cG = fromIntegral g
--        cB = fromIntegral b
--        (h, _, v) = (hsvView $ RGB cR cG cB) :: (Double, Double, Double)
--        saturated = convert (PixelRGB8 r g b)
--        sR = fromIntegral $ round $ channelRed saturated
--        sG = fromIntegral $ round $ channelGreen saturated
--        sB = fromIntegral $ round $ channelBlue saturated
convert :: PixelRGB8 -> PixelRGB8
convert (PixelRGB8 r g b) = PixelRGB8 h s v 
        where
            cMax = maximum [r,b,g]
            cMin = minimum [r,b,g]
            delta = cMax - cMin
            h | cMax == r = if (g-b) <= 0 then fromIntegral (360 + toInteger (60 * ((((g - b) `div` delta))))) else fromIntegral (60 * ((((g - b) `div` delta))))
              | cMax == g = if (b-r) <= 0 then fromIntegral (360 + toInteger (2 + (60 * ((((b - r) `div` delta)))))) else fromIntegral (60 * ((((b - r) `div` delta))))
              | cMax == b = if (r-g) <= 0 then fromIntegral (360 + toInteger (4 + (60 * ((((r - g) `div` delta)))))) else fromIntegral (60 * ((((r - g) `div` delta))))
              | otherwise = fromIntegral $ toInteger (-1)
            s | cMax == 0 = 0
              | otherwise = fromIntegral (delta `div` cMax)
            v | cMax == 0 = undefined
              | otherwise = cMax
-- we know that r,g,b are values between [0..1]
-- h = 0,360 s = [0,1] v = [0,1]