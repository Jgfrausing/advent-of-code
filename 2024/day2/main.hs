module Main where

import Data.List (sort)
import qualified Data.Bifunctor
import Control.Exception (try)

main :: IO ()
main = task1 "reports.txt"



task1 file = do
    reports <- getData file


    let increased = map tryMakeIncreasing reports
    let valid = filter reportIsValid increased

    print $ length valid


tryMakeIncreasing [] = []
tryMakeIncreasing [x] = [x]
tryMakeIncreasing (x:xs) =
    if x > head xs then reverse (x:xs)
    else x:xs

reportIsValid [] = True
reportIsValid [x] = True
reportIsValid (x:xs) =
    let next = head xs in
    ((x < next && next <= 3+x) && reportIsValid xs)

convertToDigits s = read s :: Int

getData path = do
    input <- lines <$> readFile path
    -- return each line as a list of ints 
    return $ map (map convertToDigits . words) input
