module CircleBitmapExample where
 
  import Circle
  import Bitmap
  import Control.Monad.ST
  import Codec.Picture
  import Control.Parallel.Strategies
  import Text.Printf


  drawCircle :: Point -> Int -> IO ()
  drawCircle center radius = do    
    writePng "../test/TEST.png" $ generateImage pixelRenderer w h
    where
      circlePoints = generateCirclePoints (w `div` 2,h `div` 2) 400
      pixelRenderer x y
        | (x , y) `elem` circlePoints = PixelRGB8 0 0 0 
        | otherwise                   = PixelRGB8 255 255 255 
      w = 1000
      h = 1000


-- drawCircle :: Int -> Point -> Image PixelRGBA8 
                -- radius
-- scale :: Int -> (Int, Int) -> Image PixelRGBA8 -> Image PixelRGBA8
-- we will keep doing this recursively till values of (Int,Int) are less than a particular value of our choice. 
-- Keep scaling and storing it. Then with ffmpeg we would be able to combine it

-- map (\x -> scale x x image) [100,110..400]