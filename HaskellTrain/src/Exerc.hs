{-# LANGUAGE FlexibleContexts #-}
module Exerc where

import Data.List ( sort , intersperse)
import GHC.IO.Device (IODevice(dup2))
import Data.Char
import Data.Bits (Bits(xor))
import GHC.Num (floatFromInteger)

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

added :: Maybe Integer
added = fmap (+3) (lookup 3 $ zip [1, 2, 3] [4, 5, 6])

y :: Maybe Integer
y = lookup 3 $ zip [1, 2, 3] [4, 5, 7]

z :: Maybe Integer
z = lookup 2 $ zip [1, 2, 3] [4, 5, 6]

tupled :: Maybe (Integer, Integer)
tupled = (,) <$> y <*> z

newtype Identity a = Identity a
        deriving (Eq, Ord, Show)
instance Functor Identity where
        fmap f (Identity x) = Identity (f x)
instance Applicative Identity where
        pure x= Identity x
        (<*>) (Identity f) (Identity x) = Identity (f x)


newtype Constant a b =
        Constant { getConstant :: a }
        deriving (Eq, Ord, Show)
instance Functor (Constant a) where
    fmap f (Constant a)= Constant a
instance Monoid a => Applicative (Constant a) where
    pure a = Constant mempty
    (<*>) (Constant f) (Constant c) = Constant (f `mappend` c)


percentage :: Integer -> (Integer, Integer)
percentage x = (round $ fromInteger x * 0.9  , x - round (fromInteger x * 0.9) )

percentage2 :: Integer -> (Integer, Integer)
percentage2 x = (10* div x 10 ,10 )


data Expr
    = Lit Integer
    | Add Expr Expr

eval :: Expr -> Integer
eval (Lit x) = x
eval (Add y z ) = eval y + eval z

printExpr :: Expr -> String
printExpr (Lit x)= show x
printExpr (Add y z)= (printExpr y) ++ " + " ++ (printExpr z)

notThe :: String -> Maybe String
notThe "the" = Nothing
notThe "The" = Nothing
notThe x     = Just x

theToA :: String -> String
theToA "the" = "a"
theToA "The" = "A"
theToA x     =  x

replaceThe :: String -> String
replaceThe x = unwords (map theToA (words x))

countTheBeforeVowel :: String -> Integer
countTheBeforeVowel = undefined

newtype Word' = Word' String
    deriving (Eq, Show)

vowels :: [Char]
vowels = "aeiou"

vowelsInWord :: [Char] -> [Char]
vowelsInWord [] = []
vowelsInWord (x:xs) = if x `elem` vowels then x : vowelsInWord xs else vowelsInWord xs


mkWord :: String -> Maybe Word'
mkWord xs = if length xs - length (vowelsInWord xs) > length (vowelsInWord xs) then Just (Word' xs) else Nothing


data Nat =
    Zero
    | Succ Nat
    deriving (Eq, Show)

natToInteger :: Nat -> Integer
natToInteger Zero = 0
natToInteger (Succ x) = 1 + natToInteger x

integerToNat :: Integer -> Maybe Nat
integerToNat xs = if xs < 0 then Nothing else Just (intToNat xs) where
    intToNat 0  = Zero
    intToNat x  = Succ (intToNat (x-1))

isJust :: Maybe a -> Bool
isJust Nothing = False
isJust _ = True

isNothing :: Maybe a -> Bool
isNothing Nothing = True
isNothing _ = False

mayybee :: b -> (a->b) -> Maybe a -> b
mayybee y _ Nothing = y
mayybee y f (Just x) = f x

fromMaybe :: a -> Maybe a -> a
fromMaybe x  = mayybee x id

maybeToList :: Maybe a -> [a]
maybeToList Nothing = []
maybeToList (Just x) = [x]

listToMaybe :: [a] -> Maybe a
listToMaybe [] = Nothing
listToMaybe (x:xs) = Just x

catMaybes :: [Maybe a] -> [a]
catMaybes x = [a | Just a <- x]

flipMaybe :: [Maybe a] -> Maybe [a]
flipMaybe [] = Nothing
flipMaybe (Nothing:xs) = Nothing
flipMaybe ((Just x):xs) = if length (catMaybes xs) == length xs  then Just (catMaybes xs) else Nothing

lefts' :: [Either a b] -> [a]
lefts' x = [a | Left a <- x]

rights' :: [Either a b] -> [b]
rights' x = [a | Right a <- x]

partitionEithers' :: [Either a b] -> ([a], [b])
partitionEithers' x  = (lefts' x, rights' x)

eitherMaybe' :: (b -> c) -> Either a b -> Maybe c
eitherMaybe' _ (Left _) = Nothing
eitherMaybe' f (Right x) = Just (f x)

either' :: (a -> c) -> (b -> c) -> Either a b -> c
either' f _ (Left x) = f x
either' _ g (Right x) = g x

myIterate :: (a -> a) -> a -> [a]
myIterate f x = x : myIterate f (f x)

myUnfoldr :: (b -> Maybe (a, b)) -> b -> [a]
myUnfoldr f x = case f x of
    Nothing -> []
    Just (x1,x2) -> x1 : myUnfoldr f x2


test :: Integral b => b -> Maybe (Bool, b)
test x = Just (x `mod` 3 == 0, x+1)
