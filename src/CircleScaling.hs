module CircleScaling where
  -- import Data.ByteString as B
  import Codec.Picture
  -- import Codec.Picture.ScaleDCT (scale)
  import LOL
  import Text.Printf
  -- import Vision.Image.Transform.ScaleDCT
  
  loadImage :: IO (Maybe (Image PixelRGBA8))
  loadImage = do
    imageLoad <- readPng "src/circle.png"
    return $ case imageLoad of
      Left _                   -> Nothing
      Right (ImageRGBA8 image) -> Just image
      
        
  
  scaling :: Int -> Int -> Int -> Int -> IO ()
  scaling frameNum frameCount dim step = do
    image <- loadImage
    case image of
      Nothing    -> putStrLn "error loading image"
      Just image -> genCircleFrame image frameNum frameCount dim step
  
  genCircleFrame :: (Image PixelRGBA8) -> Int -> Int -> Int -> Int -> IO()
  genCircleFrame image frameNum frameCount dim step = do
    writePng (printf "images/circle%04d.png" (frameNum+1)) (scaleBilinear dim dim image)
    if (frameNum+1) == frameCount
      then return()
      else genCircleFrame image (frameNum + 1) frameCount (dim + step) step
            -- putStrLn $ printf "rendering circle %d" (frame+1)
            -- writePng (printf "images/circle%04d.png" frame) (scaleBilinear dim dim image)

    

  
  
    
  