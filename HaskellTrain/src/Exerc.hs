module Exerc where

import Data.List ( sort , intersperse)
import GHC.IO.Device (IODevice(dup2))
import Data.Char 

m :: (x->y) -> (y->(w,z))-> x -> w
m f g x = fst (g (f x))


data TisAnInteger =
    TisAn Integer

instance Eq TisAnInteger where
   TisAn v == TisAn v' = v == v'

data TwoIntegers =
    Two Integer Integer

instance Eq TwoIntegers where
    (==) (Two x y) (Two x' y') = (x == x') && (y == y')

data StringOrInt =
    TisAnInt Int
    | TisAString String

instance Eq StringOrInt where
    (==) (TisAnInt x) (TisAnInt x') = x == x'
    (==) (TisAString y) (TisAString y') = y == y'
    (==) _ _ = False


type Subject = String
type Verb = String
type Object = String
data Sentence =
    Sentence Subject Verb Object
    deriving (Eq, Show)
s1 :: Object -> Sentence
s1 = Sentence "dogs" "drool"
s2 :: Sentence
s2 = Sentence "Julie" "loves" "dogs"

chk :: Eq b => (a -> b) -> a -> b -> Bool
chk f a b = f a == b

arith :: Num b => (a -> b) -> Integer -> a -> b
arith f n a = f a


addOneIfOdd n = case odd n of
    True -> f n
    False -> n
    where f = \x -> x+1


addFive :: Integer -> Integer -> Integer
addFive = \x -> \y -> (if x > y then y else x) + 5

tensDigit :: Integral a => a -> a
tensDigit x = d
    where xLast = x `div` 10
          d = xLast `mod` 10


tensDigit' :: Integral b => b -> b
tensDigit' x = snd (divMod (fst (divMod x 10)) 10)

hunsD :: Integral b => b -> b
hunsD x = d2
    where d2 = tensDigit' (div x 10)


foldBool3 :: a -> a -> Bool -> a
foldBool3 x y True = x
foldBool3 x y False = y

foldBool2 :: a -> a -> Bool -> a
foldBool2 x y b
    | b = x
    | otherwise = y

foldBool1 :: p -> p -> Bool -> p
foldBool1 x y b= case b of
    True -> x
    False -> y

g :: (a -> b) -> (a, c) -> (b, c)
g f (x,y) = (f x , y)

roundTrip :: (Show a, Read b) => a -> b
roundTrip  = read.show
{-main :: IO ()
main = do
print (roundTrip 4 :: Int)
print (id 4)-}


cattyConny :: String -> String -> String
cattyConny x y = x ++ " mrow " ++ y

flippy :: String -> String -> String
flippy = flip cattyConny
appedCatty :: String -> String
appedCatty = cattyConny "woops"
frappe :: String -> String
frappe = flippy "haha"

mc91 :: (Ord a, Num a) => a -> a
mc91 x
    | x > 100 = x - 10
    | otherwise = 91


digitToWord :: Int -> String
digitToWord n = case n of
    1 -> "one"
    2 -> "two"
    3 -> "three"
    4 -> "four"
    5 -> "five"
    6 -> "six"
    7 -> "seven"
    8 -> "eight"
    9 -> "nine"
    _ -> "zero"




digits :: Integral a => a -> [a]
digits n = case (divMod n 10) of
    (0,0) ->  []
    (x,y) ->  digits x ++ [y]


wordNumber :: Int -> String
wordNumber n = tail (concat (map (("-"++).digitToWord) (digits n)))

wordNumber' :: Int -> String
wordNumber' n = tail $ concatMap (("-"++).digitToWord) $ digits n



multiples3 = (length.filter (\x -> mod x 3 == 0)) [1..30]

noArticles :: String -> [String]
noArticles = filter (\x ->  x `notElem` ["a", "the", "an"]).words

myZip :: [a] -> [b] -> [(a, b)]
myZip [] ys = []
myZip xs [] = []
myZip (x:xs) (y:ys) = (x,y) : myZip xs ys

myZipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
myZipWith f xs []= [] 
myZipWith f [] ys= []   
myZipWith f (x:xs) (y:ys)= f x y : myZipWith f xs ys

myZip' :: [a] -> [b] -> [(a, b)]
myZip' = myZipWith (,)

myAny :: (a -> Bool) -> [a] -> Bool
myAny f = foldr ((||).f) False 

squishMap :: Foldable t => (a1 -> a2) -> t a1 -> [a2]
squishMap f = foldr ((:).f ) [] 



