module Main where

import Data.List (sort)
import qualified Data.Bifunctor

main :: IO ()
main = do
    putStrLn "Hello, Haskell!"
    tuples <- getTuples "list.txt"

    let asDigits = apply convertToDigits tuples
    let zipped = uncurry zip . Data.Bifunctor.bimap sort sort . unzip $ asDigits

    let summed = sum $ map (\(x, y) -> abs (x - y)) zipped

    print summed


apply :: (a -> b) -> [(a, a)] -> [(b, b)]
apply f = map (Data.Bifunctor.bimap f f)

convertToDigits s = read s :: Int

getTuples path = do
    input <- lines <$> readFile path
    return $ map (\line -> let [w1, w2] = words line in (w1, w2)) input
