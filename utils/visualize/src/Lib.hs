module Lib where

import Data.Binary.Get
import Data.Array as A
import Data.ByteString.Lazy
import Graphics.Image as I

getNTimes :: Int -> Get [Int]
getNTimes n = sequence ((fmap fromIntegral) <$> Prelude.replicate n getWord32le)
     
read2DArray :: ByteString -> Int -> A.Array (Int, Int) Int
read2DArray bs i = listArray ((0,0), (i,i)) (runGet (getNTimes i) bs)

toPixel :: A.Array (Int, Int) Int -> Int -> (Int, Int) -> I.Pixel RGB Integer
toPixel arr size (x, y) = toColor (arr ! (x `div` size, y `div` size))

toColor 1 = I.PixelRGB 0 0 0
toColor 0 = I.PixelRGB 1 1 1

genImage :: I.Array arr RGB Integer => A.Array (Int, Int) Int -> Int -> Image arr RGB Integer
genImage arr scaling = makeImage (x * scaling, y * scaling) (toPixel arr scaling)
    where ((_, _), (x, y)) = bounds arr
