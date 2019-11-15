{-# LANGUAGE RankNTypes #-}

module Section where

import Codec.Picture
import Text.Printf
import System.Random
import Data.Array
import Control.Parallel.Strategies
import DrawBitmap

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
    kick       :: Bool,
    circles    :: Int,
    cFrames    :: Int
} deriving (Show, Eq)

quarterNotesA = map (\x -> 554 + round (x * (3600.0/130.01))) [0..7421]
quarterNotesB = map (\x -> 7643 + round (x * (3600.0/130.01))) [0..10000]
chopNotesA    = [554 + round (x * (3600.0/260.02) * 4.0) | x <- [0..7421]]
chopNotesB    = [554 + round (x * (3600.0/260.02) * 4.0) | x <- [0..100000]]

-- write frame to file
genFrame :: Julia -> Int -> Int -> Bool -> IO ()
genFrame f frame cFrame doCircle = do
    writePng (printf "out/frame%05d.png" frame) frameF
    if doCircle then putStrLn "yes" else return ()
    where
        h = height f
        w = width f
        bounds     = ((0, 0), (w-1,h-1))
        pixels     = parMap rseq (uncurry (genFractal f)) (range bounds)
        radius     = round ((fromIntegral w) * (fromIntegral cFrame) / 12.0) + 1
        circle     = genCirclePoints w h radius (w `div` 12)
        overlay    = zipWith (\(PixelRGB8 r g b) c -> if c then PixelRGB8 (255-r) (255-g) (255-b) else PixelRGB8 r g b) pixels circle
        pixelArray = listArray bounds $ if doCircle then overlay else pixels
        f'         = curry (pixelArray !)
        frameF     = generateImage f' w h


genSection :: Options -> IO Options
genSection (Options frame out depth zoom zoomStep cXstep cYstep sat kick circles cFrames) = do
        -- define fractal
        let f = Julia 720 1280 zoom newcX newcY depth frame sat
        
        -- render fractal
        genFrame f frame (18 - cFrames) doCircle

        let nFrame = frame + 1

        -- re-run with new params if frames left to render
        if nFrame == out
            then return     $ Options frame out depth zoom zoomStep cXstep cYstep sat kick circles cFrames
            else genSection $ Options nFrame out newDepth newZoom zoomStep cXstep cYstep sat kick nCircles nCFrames
    where
        nFrame = fromIntegral frame
        nDepth = fromIntegral depth

        newcX  = sin $ negate nFrame * cXstep
        newcY  = sin $ negate nFrame * cYstep
        
        -- set up kick events before and after first chorus
        -- disable kick events if no kick
        kickFrames 
            | not kick = [-1]
            | otherwise = 
                if frame < 7422 then
                    quarterNotesA
                else
                    quarterNotesB
        
        chopFrames
            | frame < 7422 = chopNotesA
            | otherwise    = chopNotesB
        
        nCFrames
            | circles == 0                            = 0
            | cFrames == 0 && frame `elem` chopFrames = 18
            | cFrames == 0                            = 0
            | otherwise                               = cFrames - 1
        
        nCircles = if nCFrames == 18 then circles - 1 else circles
        
        doCircle = cFrames /= 0

        -- increase depth and zoom if kick
        newDepth = if frame `elem` kickFrames then depth + 1 else depth
        newZoom 
            | zoom > 2 = 0
            | otherwise = if frame `elem` kickFrames then zoom + (zoomStep * 10) else zoom + zoomStep

introA :: IO Options
introA = genSection (Options 0 111 10 0.3 0.0001 0 0 0 False 0 0)

introB :: Options -> IO Options
introB (Options frame _ depth zoom zoomStep cXstep cYstep s kick c cf) =
    genSection (Options frame 554 depth zoom zoomStep 0.0001 0.001 s kick c cf)

verse1 :: Options -> IO Options
verse1 (Options frame _ depth zoom zoomStep cXstep cYstep s kick c cf) =
    genSection (Options frame 4098 depth zoom zoomStep cXstep cYstep s True c cf)

bridge1A :: Options -> IO Options
bridge1A (Options frame _ depth zoom zoomStep cXstep cYstep s kick c cf) =
    genSection (Options frame 4541 depth zoom 0.0005 0.00015 0.0015 s kick c cf)

bridge1B :: Options -> IO Options
bridge1B (Options frame _ depth zoom zoomStep cXstep cYstep s kick c cf) =
    genSection (Options frame 5427 depth 0.5 0.0007 0.0002 0.002 (1/7) kick 8 24)

bridge1C :: Options -> IO Options
bridge1C (Options frame _ depth zoom zoomStep cXstep cYstep s kick c cf) =
    genSection (Options frame 5856 depth 0.5 0.001 0.00025 0.0025 (2/7) kick c cf)

chorus1 :: Options -> IO Options
chorus1 (Options frame _ depth zoom zoomStep cXstep cYstep s kick c cf) =
    genSection (Options frame 7421 depth zoom zoomStep cXstep cYstep s kick c cf)

chorusend :: Options -> IO Options
chorusend (Options frame _ depth zoom zoomStep cXstep cYstep s kick c cf) =
    genSection (Options frame 7643 depth zoom 0.0001 cXstep cYstep s False c cf)

bridge2 :: Options -> IO Options
bridge2 (Options frame _ depth zoom zoomStep cXstep cYstep s kick c cf) =
    genSection (Options frame 8086 depth zoom 0.001 cXstep cYstep s True c cf)

verse2a :: Options -> IO Options
verse2a (Options frame _ depth zoom zoomStep cXstep cYstep s kick c cf) =
    genSection (Options frame 8972 depth 0.5 0.0012 0.0003 0.003 (3/7) kick c cf)

verse2b :: Options -> IO Options
verse2b (Options frame _ depth zoom zoomStep cXstep cYstep s kick c cf) =
    genSection (Options frame 9858 depth 0.5 0.0015 0.00035 0.0035 (4/7) kick c cf)

verse2c :: Options -> IO Options
verse2c (Options frame _ depth zoom zoomStep cXstep cYstep s kick c cf) =
    genSection (Options frame 10744 depth 0.5 0.0017 0.0004 0.004 (5/7) kick c cf)

prechorus :: Options -> IO Options
prechorus (Options frame _ depth zoom zoomStep cXstep cYstep s kick c cf) =
    genSection (Options frame 11187 depth 0.5 0.0017 0.0004 0.004 (6/7) kick c cf)

chorus2 :: Options -> IO Options
chorus2 (Options frame _ depth zoom zoomStep cXstep cYstep s kick c cf) =
    genSection (Options frame 13402 depth 0.06 0.004 0.0008 0.008 (7/7) kick c cf)

outro :: Options -> IO Options
outro (Options frame _ depth zoom zoomStep cXstep cYstep s kick _ _) =
    genSection (Options frame 14122 depth zoom (-0.0005) 0 0 0 False 0 0)