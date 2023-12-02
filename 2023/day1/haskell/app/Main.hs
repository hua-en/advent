module Main where

import qualified Data.Text as T
import qualified Data.Text.IO as T.IO
import Data.Char

main :: IO ()
-- main = do
--     contents <- T.IO.readFile "day1input.txt"
--     let filelines = T.lines contents
--     let totsum = sum $ getnumber . T.filter isDigit <$> filelines
--     print totsum
main = print . numberCount =<< T.IO.readFile "day1input.txt"

getNumber :: T.Text -> Int
getNumber txt = read [T.head txt, T.last txt]

numberCount :: T.Text -> Int
numberCount = sum . fmap (getNumber . T.filter isDigit) . T.lines