{-
 - Generates the rules for Conway's GOL
-}
module Main where

import qualified Data.ByteString.Lazy as BL
import Data.Binary.Put

putHeader :: Put
putHeader = do
    putWord32le 512
    putWord32le 9

putBody :: Put
putBody = sequence_ ((putWord32le . fromIntegral) <$> (concat genRules))
    

genConfigs :: Integer -> Integer -> [[Integer]]
genConfigs 1 j = (\x -> [x]) <$> [0..j]
genConfigs i j = [0..j] >>= (\x -> (fmap (x :) (genConfigs (i-1) j)))

getState :: [Integer] -> Integer
getState is
    | (cell == 1) && (neighbors == 2 || neighbors == 3) = 1
    | (cell == 0) && (neighbors == 3)                   = 1
    | otherwise                                         = 0 
    where 
        neighbors = (length . (filter (== 1)) . tail) is
        cell = head is

genRules :: [[Integer]]
genRules = (\x -> x ++ [getState x]) <$> genConfigs 9 1

main :: IO ()
main = do
    BL.writeFile "gol.table" (runPut (putHeader >> putBody))
