{-# LANGUAGE RankNTypes #-}

module Section (
    introA,
    introB,
    verseA
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
    count      :: Int,
    depth      :: Int,
    zoom       :: Double,
    zoomStep   :: Double,
    cX         :: Double,
    cY         :: Double,
    cXstep     :: Double,
    cYstep     :: Double,
    saturation :: Double,
    kick       :: Bool
} deriving (Show, Eq)

quarterNotes = map (\x -> 570 + round (x * (3600.0/130.0))) [0..20000]

genFrame :: Julia -> Int -> Double -> IO ()
genFrame f frame sat = do
    -- write frame to file
    writePng (printf "out/frame%04d.png" frame) frameF
    putStrLn $ printf "Rendering frame %d" (frame + 1)
    where
        h = height f
        w = width f
        bounds     = ((0, 0), (w-1,h-1))
        pixels     = parMap rseq (uncurry (genFractal f)) (range bounds)
        saturated  = map (Processing.saturation sat) pixels
        pixelArray = listArray bounds pixels
        f'         = curry (pixelArray !)
        frameF     = generateImage f' w h

genSection :: Options -> IO Options
genSection (Options frame frameCount depth zoom zoomStep cX cY cXstep cYstep sat kick) = do
        -- define fractal
        let f = Julia 200 200 zoom newcX newcY depth
        
        -- render fractal
        genFrame f frame sat

        -- re-run with new params if frames left to render
        if (frame + 1) == frameCount
        then 
            return $ Options frame frameCount depth zoom zoomStep cX cY cXstep cYstep sat kick
        else 
            genSection $ Options (frame + 1) frameCount newDepth newZoom zoomStep newcX newcY cXstep cYstep sat kick
    where
        nFrame = fromIntegral frame
        nDepth = fromIntegral depth

        newcX  = if nFrame >= 0 && nFrame <= 124 then sin $ cX + nFrame * cXstep else sin $ nFrame * cXstep 
        newcY  = if nFrame >= 0 && nFrame <= 124 then sin $ cY + nFrame * cYstep else sin $ nFrame * cYstep

        -- disable kick events if no kick
        kickFrames = if kick then quarterNotes else [-1]

        -- increase depth and zoom if kick
        newDepth = if frame `elem` kickFrames then depth + 1 else depth
        newZoom 
            | zoom > 3 = 0.5
            | otherwise = if frame `elem` kickFrames then zoom + (zoomStep * 10) else zoom + zoomStep

introA :: IO Options
introA = genSection (Options 0 126 10 0.3 0.0001 0 0.3 0 0 0 False)

introB :: Options -> IO Options
introB (Options frame _ depth zoom zoomStep cX cY _ _ s _) =
    genSection (Options frame (444 + frame) depth zoom zoomStep cX cY 0.0001 0.001 s False)

verseA :: Options -> IO Options
verseA (Options frame _ depth zoom zoomStep cX cY cXstep cYstep s _) =
    genSection (Options frame (3150 + frame) depth zoom zoomStep cX cY cXstep cYstep s True)