module Lib
    ( someFunc
    ) where

--import Test.QuickCheck (quickCheck)

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

-- Experssions
data Expr = Val Int | Add Expr Expr
                    | Mult Expr Expr



eval :: Expr -> Int
eval (Val n) = n 
eval (Add x y) = eval x + eval y
eval (Mult x y) = eval x * eval y 

-- Binary tree
data Tree   = Leaf Int
            | Node Int Tree Tree

-- Example
tree :: Tree
tree = Node 5   (Leaf 4)
                (Node 3 (Leaf 2)
                        (Leaf 1))

occurs :: Int -> Tree -> Bool
occurs m (Leaf n) = m == n
occurs m (Node n t1 t2) = m == n || occurs m t1 || occurs m t2 

tolist :: Tree -> [Int]
tolist (Leaf n) = [n]
tolist (Node n t1 t2) = [n] 
                       ++ tolist t1 
                       ++ tolist t2   

-- Data types

factors :: Integral a => a -> [a]
factors n = [x | x <- [2..n-1], n `mod` x == 0]

type Pos = (Int,Int)
origin :: Pos
origin = (0,0)

left :: Pos -> Pos
left (x,y) = (x-1,y)

data Answer = Yes | No | Unknown
        deriving Show

answers :: [Answer]
answers = [Yes,No,Unknown]

flip2 :: Answer -> Answer
flip2 Yes = No
flip2 No = Yes
flip2 Unknown = Unknown


data Maybex a = Nothingx | Justx a
        deriving Show
safediv :: Int -> Int -> Maybex Int
safediv m 0 = Nothingx
safediv m n = Justx (div m n)

isJustx :: Maybex a -> Bool
isJustx Nothingx = False
isJustx (Justx _) = True

fromJustx :: Maybex p -> p
fromJustx Nothingx = error "shouldn't happen"
fromJustx (Justx x) = x

divFrom10x :: Int -> [ Int]
divFrom10x n =  [ fromJustx(safediv 10 x) | x <- [n..50] , isJustx (safediv 10 x) ]

-- data types cont

data StudentID = StudentID {
                   -- | 
                   macId :: String,
                   -- | 
                   studentNum :: String} {-pattern matched macId studentNum, to pull out data -}
    deriving Show
student1 :: StudentID
student1 = StudentID "jimmy" "00001"

-- recursive algos

bubble :: Ord a => [a] -> [a]
bubble (x0:x1:xs)
    | x0 > x1 = x1:bubble (x0:xs) 
    | otherwise = x0:bubble (x1:xs)
bubble xs = xs

bubbleSort :: Ord a => [a] -> [a]
bubbleSort xs = 
    let 
        xs' = bubble xs
    in if xs' == xs
        then xs
        else bubbleSort xs'

insertion :: Ord t => t -> [t] -> [t]
insertion y [] = [y] 
insertion y (x:xs) 
    | y < x = y:(x:xs)
    | otherwise = x : insertion y xs

insertionSort :: Ord t => [t] -> [t]
insertionSort [] = []
insertionSort (x:xs) = insertion x (insertionSort xs)
 
qsort :: Ord a => [a] -> [a]
qsort [] = []
qsort (p:xs) =
    let
        lesser = filter (<p ) xs
        greater = [ x | x<-xs, x > p]
    in qsort lesser ++ [p] ++ qsort greater
 
-- I/O

printHello :: IO ()
printHello = print "HiWorld"

printSomeStuff :: IO()
printSomeStuff = do putStr "some "
                    putStrLn "stuff"

makeAFile :: IO()
makeAFile = do writeFile "file.txt" "some stuff\n"
               appendFile "file.txt" " other stuff" 

echo :: IO ()
echo = do line <- getLine
          print line

echoFile :: IO ()
echoFile = do file <- readFile "file.txt"
              putStrLn file