module Main where

import qualified Data.Text as T
import qualified Data.Text.IO as T.IO
import Data.Char

main :: IO ()
main = T.IO.putStrLn =<< T.IO.readFile "day4test.txt"