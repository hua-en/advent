module Main where

import qualified Data.Text as T
import qualified Data.Text.IO as T.IO

main :: IO ()
main = do
    contents <- T.IO.readFile "test.txt"
    let filelines = T.lines contents
    mapM_ T.IO.putStrLn filelines