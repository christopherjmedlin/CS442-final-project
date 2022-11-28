module Main (main) where

import Lib
import System.Environment
import Graphics.Image
import Data.ByteString.Lazy as BS hiding (head, tail)

main :: IO ()
main = do
    args <- getArgs
    let path = head args
    let scaling = (read.head.tail) args 
    let size = (read.head.tail.tail) args
    bs <- BS.readFile path
    let arr = read2DArray bs size 
    let im = genImage arr scaling
    writeImage "out.png" im
