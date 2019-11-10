{-# LANGUAGE RankNTypes #-}

module Main where

import Codec.Picture
import Text.Printf
import System.Random
import Data.Array
import Control.Parallel.Strategies

import Fractal

quarterNotes = map (\x -> round (x * (3600.0/130.0))) [0..20000]
sixteenthNotes = map (\x -> round (x * ((3600.0/130.0)/16.0))) [0..20000]

genFrame :: Julia -> Int -> Image PixelRGB8
genFrame f frame = generateImage f' w h
  where
    h = height f
    w = width f
    bounds     = ((0, 0), (w-1,h-1))
    pixels     = parMap rseq (uncurry (genFractal f)) (range bounds)
    pixelArray = listArray bounds pixels
    f'         = curry (pixelArray !)

genFrames :: Int -> Int -> Int -> Double -> IO ()
genFrames frame frameCount depth zoom
  | frame == frameCount = return ()
  | otherwise = do
    -- get random offsets for cX cY idk
    randA <- randomRIO (0.04, 0.05) :: IO Double
    randB <- randomRIO (0.04, 0.05) :: IO Double

    -- define fractal
    let f = Julia 200 200 zoom (sin (nFrame * 0.0001)) (sin (nFrame * 0.001)) depth

    -- write frame to file
    writePng (printf "out/frame%04d.png" frame) $ genFrame f frame
    putStrLn $ printf "Rendering frame %d/%d" (frame + 1) frameCount

    -- re-run with new params
    genFrames (frame + 1) frameCount newDepth newZoom
  where
    nFrame = fromIntegral frame
    nDepth = fromIntegral depth

    -- increase depth and zoom if kick
    newDepth = if frame `elem` quarterNotes then depth + 1 else depth
    newZoom 
      | zoom > 3 = 0.5
      | otherwise = if frame `elem` quarterNotes then zoom + 0.01 else zoom + 0.001
    
main :: IO ()
main = do
  genFrames 0 1000 1000 0.5
  putStrLn "done"
