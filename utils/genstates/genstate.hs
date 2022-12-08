{-
 - Generates random n x n grid
-}
module Main where

import qualified Data.ByteString.Lazy as BL
import Data.Binary.Put
import System.Random
import Control.Monad.State.Lazy
import System.Environment

randWord :: Integer -> StateT StdGen PutM ()
randWord 0 = return ()
randWord k = do
    g <- get
    let (w, newg) = genWord32R 1 g
    put newg
    lift $ putWord32le w
    randWord (k - 1) 

main :: IO ()
main = do
    g <- newStdGen
    args <- getArgs
    let n = read (head args)
    BL.writeFile "state.out" (runPut (evalStateT (randWord (n * n)) g))
