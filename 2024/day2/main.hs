module Main where

import Data.List (sort, transpose)
import qualified Data.Bifunctor
import Control.Exception (try)

main :: IO ()
main = task2 "reports.txt"


task1 file = do
    reports <- getData file

    let increased = map tryMakeIncreasing reports
    let valid = filter reportIsValid increased

    print $ length valid


task2 file = do
    reports <- getData file
    let reportsRevered = map reverse reports

    let validIncreasing= map reportIsValidWithDamper reports
    let validDecreasing= map reportIsValidWithDamper reportsRevered

    let zipped = zip validIncreasing validDecreasing

    let valid = filter (uncurry (||)) zipped

    print $ length valid


tryMakeIncreasing [] = []
tryMakeIncreasing [x] = [x]
tryMakeIncreasing (x:xs) =
    let ii = head xs in
    if x > ii then reverse (x:xs)
    else x:xs

reportIsValidWithDamper x = reportIsValidWithDamper_ (x, False) || reportIsValidWithDamper_ (tail x, True)

reportIsValidWithDamper_ ([], damperRemoved)  = True
reportIsValidWithDamper_ ([x], damperRemoved) = True
reportIsValidWithDamper_ (x:xs, False) = 
    let next = head xs in
    let valid = x < next && next <= 3 + x in
    
    if valid then reportIsValidWithDamper_ (xs, False)
       else let new = x : tail xs in reportIsValidWithDamper_ (new, True) 
reportIsValidWithDamper_ (x:xs, True) =
    let next = head xs in
    let valid = x < next && next <= 3 + x in
    valid && reportIsValidWithDamper_ (xs, True)


reportIsValid :: (Ord a, Num a) => [a] -> Bool
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
