module Main where

import Data.List (sort)
import qualified Data.Bifunctor

main :: IO ()
main = task2 "list.txt"

task2 file = do
    tuples <- getTuples file 

    let asDigits = apply convertToDigits tuples
    let (a, b) = unzip asDigits

    let score = sum $ map (\x -> x * length (filter (==x) b)) a

    print score 



task1 file = do
    tuples <- getTuples file

    let asDigits = apply convertToDigits tuples
    let sorted = uncurry zip . Data.Bifunctor.bimap sort sort . unzip $ asDigits

    let difference = sum $ map (\(x, y) -> abs (x - y)) sorted

    print difference 


apply f = map (Data.Bifunctor.bimap f f)

convertToDigits s = read s :: Int

getTuples path = do
    input <- lines <$> readFile path
    return $ map (\line -> let [w1, w2] = words line in (w1, w2)) input
