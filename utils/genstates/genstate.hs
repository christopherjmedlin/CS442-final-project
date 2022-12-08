{-
 - Generates the rules for Conway's GOL
-}
module Main where

import qualified Data.ByteString.Lazy as BL
import Data.Binary.Put
import System.Random
import Control.Monad.State.Lazy

randWord :: StateT StdGen PutM ()
randWord = do
    g <- get
    let (w, g) = genWord32R 1 g
    put g
    lift $ putWord32le w

main :: IO ()
main = do
    g <- newStdGen
    BL.writeFile "gol.table" (runPut (evalStateT randWord g))
