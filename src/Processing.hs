module Processing where

import Codec.Picture
import Data.Colour.RGBSpace.HSV
import Data.Colour.RGBSpace
import GHC.Word

saturation :: Double -> PixelRGB8 -> PixelRGB8 
saturation s (PixelRGB8 r g b) = PixelRGB8 sR sG sB
    where 
        cR = fromIntegral r
        cG = fromIntegral g
        cB = fromIntegral b
        (h, _, v) = (hsvView $ RGB cR cG cB) :: (Double, Double, Double)
        saturated = hsv h s v
        sR = fromIntegral $ round $ channelRed saturated
        sG = fromIntegral $ round $ channelGreen saturated
        sB = fromIntegral $ round $ channelBlue saturated