module Lib
    ( someFunc
    ) where

someFunc :: IO ()
someFunc = putStrLn "Hello"

double :: Num a => a -> a
double x = x + x
quadruple :: Num a => a -> a
quadruple x = double (double x)

factorial :: (Num a, Enum a) => a -> a
factorial n = product [1 .. n]

average :: Foldable t => t Int -> Int
average ns = sum ns `div` length ns

poly :: Num a => a -> a
poly x = let
    y = x+1
    in y * y

sumEvenOdds :: Integral b => [b] -> b
sumEvenOdds xs = sums (incr (evens xs))
    where
        sums xs = foldr (+) 0 xs
        incr xs = map (+1) xs
        evens xs = filter (\x -> x `mod` 2==0) xs

add :: Int -> Int -> Int
add x y = x + y

add5 :: Int -> Int
add5 = add 5

pow :: (Eq t, Num t, Num p) => p -> t -> p
pow m 0 = 1
pow m n = m * pow m (n-1)


data Nat = Zero | Succ Nat
    deriving Show

zero :: Nat
zero = Zero
one :: Nat
one = Succ Zero
two :: Nat
two = Succ one

nat2int :: Nat -> Int
nat2int Zero     = 0
nat2int (Succ n) = 1 + nat2int n

int2nat :: Int -> Nat
int2nat 0 = Zero
int2nat x = Succ (int2nat (x-1))

addd :: Nat -> Nat -> Nat
addd Zero n     = n
addd (Succ m) n = Succ (addd m n)