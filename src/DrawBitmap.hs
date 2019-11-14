module DrawBitmap where
 
  import Circle
  import Bitmap
  import Control.Monad.ST
  import Codec.Picture
  import Control.Parallel.Strategies
  import Text.Printf

  -- legacy code by Filip
  drawCircle :: Int -> Int -> Int -> IO()
  drawCircle frameSize radius thickness = do    
    writePng "../test/TEST.png" $ generateImage pixelRenderer frameSize frameSize
    where
      centre = (frameSize `div` 2,frameSize `div` 2)
      circlePoints = concatMap (generateCirclePoints centre) [(radius-(thickness `div` 2))..(radius + (thickness `div` 2))]
      pixelRenderer x y | (x,y) `elem` circlePoints   = PixelRGB8 0   0   0
                        | otherwise                   = PixelRGB8 255 255 255

