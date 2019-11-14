module CircleScaling where

import Codec.Picture
import Processing
import Text.Printf

data Circle = Circle {
  framenuM :: Int,
  frameCount :: Int,
  dim :: Int,
  step :: Int
}

loadImage :: IO (Maybe (Image PixelRGB8))
loadImage = do
  imageLoad <- readPng "src/circle.png"
  return $ case imageLoad of
    Left _                  -> Nothing
    Right (ImageRGB8 image) -> Just image
    
      

scaling :: Circle -> IO ()
scaling circle = do
  image <- loadImage
  case image of
    Nothing    -> putStrLn "error loading image"
    Just image -> genCircleFrame image circle

genCircleFrame :: Image PixelRGB8 -> Circle -> IO()
genCircleFrame image (Circle frameNum frameCount dim step) = do
  writePng (printf "images/circle%04d.png" (frameNum+1)) (scaleBilinear dim dim image)
  if (frameNum+1) == frameCount
    then return()
    else genCircleFrame image (Circle (frameNum + 1) frameCount (dim + step) step)
          -- putStrLn $ printf "rendering circle %d" (frame+1)
          -- writePng (printf "images/circle%04d.png" frame) (scaleBilinear dim dim image)

  



  
