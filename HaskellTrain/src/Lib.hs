module Lib
    ( someFunc
    ) where

someFunc :: IO ()
someFunc = putStrLn "Hello"

double x = x + x
quadruple x = double (double x)

factorial n = product [1 .. n]

average ns = sum ns `div` length ns

poly x = let
    y = x+1
    in y * y

sumEvenOdds xs = sums (incr (evens xs))
    where
        sums xs = foldr (+) 0 xs
        incr xs = map (+1) xs
        evens xs = filter (\x -> x `mod` 2==0) xs