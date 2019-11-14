{-# LANGUAGE CPP                 #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Processing where

import Codec.Picture
import Data.Colour.RGBSpace.HSV
import Data.Colour.RGBSpace
import GHC.Word
import Data.List

import Control.Monad.ST
import qualified Codec.Picture.Types as M
  
hsvToRGB :: Double -> Double -> Double -> PixelRGB8
hsvToRGB h s v = PixelRGB8 r g b
  where
    hh = h / 60.0
    i = fromIntegral $ floor hh
    ff = hh - i
    p = fromIntegral $ round $ 255 * v * (1.0 - s)
    q = fromIntegral $ round $ 255 * v * (1.0 - (s * ff))
    t = fromIntegral $ round $ 255 * v * (1.0 - (s * (1.0 - ff)))
    nV = fromIntegral $ round $ 255 * v
    (r, g, b) = case i of
      0.0 -> (nV,  t,  p)
      1.0 -> ( q, nV,  p)
      2.0 -> ( p, nV,  t)
      3.0 -> ( p,  q, nV)
      4.0 -> ( t,  p, nV)
      _   -> (nV,  p,  q)

-- end of own work

-- |
-- Module      :  Codec.Picture.Extra
-- Copyright   :  © 2016–2019 Mark Karpov
-- License     :  BSD 3 clause
--
-- Maintainer  :  Mark Karpov <markkarpov92@gmail.com>
-- Stability   :  experimental
-- Portability :  portable
--
-- Utilities for image transformation with JuicyPixels.

-- | Scale an image using bi-linear interpolation.

scaleBilinear
  :: ( Pixel a
     , Bounded (PixelBaseComponent a)
     , Integral (PixelBaseComponent a)
     )
  => Int               -- ^ Desired width
  -> Int               -- ^ Desired height
  -> Image a           -- ^ Original image
  -> Image a           -- ^ Scaled image
scaleBilinear width height img@Image {..} = runST $ do
  mimg <- M.newMutableImage width height
  let sx, sy :: Float
      sx = fromIntegral imageWidth  / fromIntegral width
      sy = fromIntegral imageHeight / fromIntegral height
      go x' y'
        | x' >= width = go 0 (y' + 1)
        | y' >= height = M.unsafeFreezeImage mimg
        | otherwise = do
            let xf = fromIntegral x' * sx
                yf = fromIntegral y' * sy
                x, y :: Int
                x  = floor xf
                y  = floor yf
                δx = xf - fromIntegral x
                δy = yf - fromIntegral y
                pixelAt' i j =
                  if i >= imageWidth || j >= imageHeight
                    then toBlack (pixelAt img 0 0)
                    else pixelAt img i j
            writePixel mimg x' y' $
              mulp (pixelAt' x y) ((1 - δx) * (1 - δy)) `addp`
              mulp (pixelAt' (x + 1) y) (δx * (1 - δy)) `addp`
              mulp (pixelAt' x (y + 1)) ((1 - δx) * δy) `addp`
              mulp (pixelAt' (x + 1) (y + 1)) (δx * δy)
            go (x' + 1) y'
  go 0 0

#define scaleBilinear_spec(pixel) \
{-# SPECIALIZE scaleBilinear :: Int -> Int -> Image pixel -> Image pixel #-}

scaleBilinear_spec(M.PixelRGBA16)
scaleBilinear_spec(M.PixelRGBA8)
scaleBilinear_spec(M.PixelCMYK16)
scaleBilinear_spec(M.PixelCMYK8)
scaleBilinear_spec(M.PixelYCbCr8)
scaleBilinear_spec(M.PixelRGB16)
scaleBilinear_spec(M.PixelYCbCrK8)
scaleBilinear_spec(M.PixelRGB8)
scaleBilinear_spec(M.PixelYA16)
scaleBilinear_spec(M.PixelYA8)
scaleBilinear_spec(M.Pixel32)
scaleBilinear_spec(M.Pixel16)
scaleBilinear_spec(M.Pixel8)

toBlack :: Pixel a => a -> a
toBlack = colorMap (const 0)
{-# INLINE toBlack #-}

mulp :: (Pixel a, Integral (PixelBaseComponent a)) => a -> Float -> a
mulp pixel x = colorMap (floor . (* x) . fromIntegral) pixel
{-# INLINE mulp #-}

addp
  :: forall a. ( Pixel a
               , Bounded (PixelBaseComponent a)
               , Integral (PixelBaseComponent a)
               ) => a -> a -> a
addp = mixWith (const f)
  where
    f x y = fromIntegral $
      (maxBound :: PixelBaseComponent a) `min` (fromIntegral x + fromIntegral y)
{-# INLINE addp #-}

-- | Crop a given image. If supplied coordinates are greater than size of
-- original image, image boundaries are used instead.

crop :: Pixel a
  => Int               -- ^ Index (X axis) of first pixel to include
  -> Int               -- ^ Index (Y axis) of first pixel to include
  -> Int               -- ^ Width of resulting image
  -> Int               -- ^ Height of resulting image
  -> Image a           -- ^ Original image
  -> Image a           -- ^ Cropped image
crop x' y' w' h' img@Image {..} =
  generateImage gen w h
  where
    gen i j = pixelAt img (x + i) (y + j)
    x = min (imageWidth  - 1) x'
    y = min (imageHeight - 1) y'
    w = min (imageWidth  - x) w'
    h = min (imageHeight  - y) h'
{-# INLINEABLE crop #-}

-- | Flip an image horizontally.

flipHorizontally :: Pixel a => Image a -> Image a
flipHorizontally img@Image {..} =
  generateImage gen imageWidth imageHeight
  where
    gen x = pixelAt img (imageWidth - 1 - x)
{-# INLINEABLE flipHorizontally #-}

-- | Flip an image vertically.

flipVertically :: Pixel a => Image a -> Image a
flipVertically img@Image {..} =
  generateImage gen imageWidth imageHeight
  where
    gen x y = pixelAt img x (imageHeight - 1 - y)
{-# INLINEABLE flipVertically #-}

-- | Rotate an image to the left by 90°.

rotateLeft90 :: Pixel a => Image a -> Image a
rotateLeft90 img@Image {..} =
  generateImage gen imageHeight imageWidth
  where
    gen x y = pixelAt img (imageWidth - 1 - y) x
{-# INLINEABLE rotateLeft90 #-}

-- | Rotate an image to the right by 90°.

rotateRight90 :: Pixel a => Image a -> Image a
rotateRight90 img@Image {..} =
  generateImage gen imageHeight imageWidth
  where
    gen x y = pixelAt img y (imageHeight - 1 - x)
{-# INLINEABLE rotateRight90 #-}

-- | Rotate an image by 180°, i.e flip both vertically and horizontally.
--
-- @since 0.2.0

rotate180 :: Pixel a => Image a -> Image a
rotate180 img@(Image w h _) = generateImage g w h
  where
    g x y = pixelAt img (w - 1 - x) (h - 1 - y)
{-# INLINEABLE rotate180 #-}

-- | Create an image by placing several images side by side. If the images
-- are of differnet heights the smallest height is used.
--
-- @since 0.2.0

beside :: Pixel a => [Image a] -> Image a
beside = foldl1' go
  where
    go :: Pixel a => Image a -> Image a -> Image a
    go img1@(Image w1 h1 _) img2@(Image w2 h2 _) =
      generateImage g (w1 + w2) h
      where
        g x
          | x < w1 = pixelAt img1 x
          | otherwise = pixelAt img2 (x - w1)
        h = min h1 h2
{-# INLINEABLE beside #-}

-- | Create an image by placing several images in a vertical stack. If the
-- images are of differnet widths the smallest width is used.
--
-- @since 0.2.0

below :: Pixel a => [Image a] -> Image a
below = foldl1' go
  where
    go :: Pixel a => Image a -> Image a -> Image a
    go img1@(Image w1 h1 _) img2@(Image w2 h2 _) =
      generateImage g w (h1 + h2)
      where
        g x y
          | y < h1 = pixelAt img1 x y
          | otherwise = pixelAt img2 x (y - h1)
        w = min w1 w2
{-# INLINEABLE below #-}