{-# LANGUAGE RankNTypes #-}

module Section (
    introA,
    introB,
    verse1,
    bridge1A,
    bridge1B,
    bridge1C,
    chorus1,
    chorusend,
    bridge2,
    verse2a,
    verse2b,
    verse2c,
    prechorus,
    chorus2,
    outro
) where

import Codec.Picture
import Text.Printf
import System.Random
import Data.Array
import Control.Parallel.Strategies

import Fractal
import qualified Processing

data Options = Options { 
    frame      :: Int,
    out        :: Int,
    depth      :: Int,
    zoom       :: Double,
    zoomStep   :: Double,
    cXstep     :: Double,
    cYstep     :: Double,
    saturation :: Double,
    kick       :: Bool
} deriving (Show, Eq)

quarterNotesA = map (\x -> 554 + round (x * (3600.0/130.0))) [0..7421]
quarterNotesB = map (\x -> 7643 + round (x * (3600.0/130.0))) [0..10000]

genFrame :: Julia -> Int -> IO ()
genFrame f frame = do
    -- write frame to file
    writePng (printf "out/frame%04d.png" frame) frameF
    if frame `mod` 10 == 0 
        then putStrLn $ printf "Rendering frame %d" (frame + 1) 
        else return()
    where
        h = height f
        w = width f
        bounds     = ((0, 0), (w-1,h-1))
        pixels     = parMap rseq (uncurry (genFractal f)) (range bounds)
        pixelArray = listArray bounds pixels
        f'         = curry (pixelArray !)
        frameF     = generateImage f' w h

        -- bounds = ((0, 0), (width-1,height-1))
        -- pixels = [0,1,1...]
        -- pixelArray = listArray bounds pixels
genSection :: Options -> IO Options
genSection (Options frame out depth zoom zoomStep cXstep cYstep sat kick) = do
        -- define fractal
        let f = Julia 200 200 zoom newcX newcY depth frame sat
        
        -- render fractal
        genFrame f frame

        -- re-run with new params if frames left to render
        if (frame + 1) == out
        then 
            return $ Options frame out depth zoom zoomStep cXstep cYstep sat kick
        else 
            genSection $ Options (frame + 1) out newDepth newZoom zoomStep cXstep cYstep sat kick
    where
        nFrame = fromIntegral frame
        nDepth = fromIntegral depth

        newcX  = sin $ nFrame * cXstep
        newcY  = sin $ nFrame * cYstep
        
        -- set up kick events before and after first chorus
        -- disable kick events if no kick
        kickFrames 
            | not kick = [-1]
            | otherwise = 
                if frame < 7422 then
                    quarterNotesA
                else
                    quarterNotesB

        -- increase depth and zoom if kick
        newDepth = if frame `elem` kickFrames then depth + 1 else depth
        newZoom 
            | zoom > 3 = 0.5
            | otherwise = if frame `elem` kickFrames then zoom + (zoomStep * 10) else zoom + zoomStep

introA :: IO Options
introA = genSection (Options 0 115 10 0.3 0.0001 0 0 0 False)

introB :: Options -> IO Options
introB (Options frame _ depth zoom zoomStep _ _ s _) =
    genSection (Options frame 554 depth zoom zoomStep 0.0001 0.001 0 False)

verse1 :: Options -> IO Options
verse1 (Options frame _ depth zoom zoomStep cXstep cYstep s _) =
    genSection (Options frame 4098 depth zoom zoomStep cXstep cYstep s True)

bridge1A :: Options -> IO Options
bridge1A (Options frame _ depth zoom _ _ _ s kick) =
    genSection (Options frame 4541 depth zoom 0.0005 0.00015 0.0015 s kick)

bridge1B :: Options -> IO Options
bridge1B (Options frame _ depth _ _ _ _ s kick) =
    genSection (Options frame 5427 depth 0.5 0.0007 0.0002 0.002 (1/7) kick)

bridge1C :: Options -> IO Options
bridge1C (Options frame _ depth _ _ _ _ s kick) =
    genSection (Options frame 5856 depth 0.5 0.001 0.00025 0.0025 (2/7) kick)

chorus1 :: Options -> IO Options
chorus1 (Options frame _ depth zoom zoomStep cXstep cYstep s kick) =
    genSection (Options frame 7421 depth zoom zoomStep cXstep cYstep s kick)

chorusend :: Options -> IO Options
chorusend (Options frame _ depth zoom zoomStep cXstep cYstep s kick) =
    genSection (Options frame 7643 depth zoom 0.0001 cXstep cYstep s False)

bridge2 :: Options -> IO Options
bridge2 (Options frame _ depth zoom zoomStep cXstep cYstep s kick) =
    genSection (Options frame 8086 depth zoom 0.001 cXstep cYstep s True)

verse2a :: Options -> IO Options
verse2a (Options frame _ depth zoom zoomStep cXstep cYstep s kick) =
    genSection (Options frame 8972 depth 0.5 0.0012 0.0003 0.003 (3/7) kick)

verse2b :: Options -> IO Options
verse2b (Options frame _ depth zoom zoomStep cXstep cYstep s kick) =
    genSection (Options frame 9858 depth 0.5 0.0015 0.00035 0.0035 (4/7) kick)

verse2c :: Options -> IO Options
verse2c (Options frame _ depth zoom zoomStep cXstep cYstep s kick) =
    genSection (Options frame 10744 depth 0.5 0.0017 0.0004 0.004 (5/7) kick)

prechorus :: Options -> IO Options
prechorus (Options frame _ depth zoom zoomStep cXstep cYstep s kick) =
    genSection (Options frame 11187 depth 0.5 0.0017 0.0004 0.004 (6/7) kick)

chorus2 :: Options -> IO Options
chorus2 (Options frame _ depth zoom zoomStep cXstep cYstep s kick) =
    genSection (Options frame 13402 depth 0.06 0.004 0.0008 0.008 (7/7) kick)

outro :: Options -> IO Options
outro (Options frame _ depth zoom zoomStep cXstep cYstep s kick) =
    genSection (Options frame 14122 depth zoom (-0.0005) 0 0 0 False)