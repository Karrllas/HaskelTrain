module Main where

import Test.Hspec
import Test.QuickCheck
--import qualified Data.Map as M
import Control.Monad (forever, when)
import Data.List (intercalate, sort)
import Data.Traversable (traverse)
import System.Environment (getArgs)
import System.Exit (exitFailure, exitSuccess)
import System.IO (hGetLine, hIsEOF, stdin, isEOF)
import GHC.IO.FD (stdin)

main :: IO ()
main = hspec $ do
    describe "Addition" $ do
        it "1 + 1 is greater than 1" $ do
            (1 + 1) > 1 `shouldBe` True
        it "2 + 2 is equal to 4" $ do
            2 + 2 `shouldBe` 4
        it "Division" $ do
            dividedBy 8 2  `shouldBe` (4, 0)
        it "Mult" $ do
            myMult 8 3  `shouldBe` 24
        it "x + 1 is always greater than x" $ do
            property $ \x -> x + 1 > (x :: Int)

runQc :: IO ()
runQc = do
    quickCheck halfIdentity
    quickCheck ((listOrdered.sort) :: [Int]->Bool)
    quickCheck (plusAssociative :: Int->Int->Int->Bool)
    quickCheck (plusCommutative :: Int->Int->Bool)
    quickCheck (law1 :: Int->Int->Bool)
    quickCheck (law2 :: Int->Int->Bool)
    quickCheck property1

genGInt :: Gen Integer
genGInt = elements [1..]
{-main :: IO ()
main = do
    mode <- getArgs
    case mode of
        [arg] ->
            case arg of
                "from" -> convertFromMorse
                "to" -> convertToMorse
                _ -> argError
        _ -> argError
    where argError = do
            putStrLn "Please specify the first argument \
            \as being 'from' or 'to' morse,\
            \ such as: morse to"
            exitFailure-}

dividedBy :: Integral a => a -> a -> (a, a)
dividedBy num denom = go num denom 0
    where go n d count
            | n < d = (count, n)
            | otherwise = go (n - d) d (count + 1)

myMult :: (Eq a, Num a) => a -> a -> a
myMult x 1 = x
myMult x y = x + myMult x (y-1)

prop_additionGreater :: Int -> Int -> Bool
prop_additionGreater x y = dividedBy x y == (div x y,mod x y)



oneThroughThree :: Gen Int
oneThroughThree = elements [1, 2, 3]

genBool :: Gen Bool
genBool = choose (False, True)

genBool' :: Gen Bool
genBool' = elements [False, True]

genOrdering :: Gen Ordering
genOrdering = elements [LT, EQ, GT]

genChar :: Gen Char
genChar = choose ('a','e')

genInt :: Gen Int
genInt = choose (1,3)

genTuple :: (Arbitrary a, Arbitrary b) => Gen (a,b)
genTuple = do
    x <- arbitrary
    y <- arbitrary
    return (x,y)

genEither :: (Arbitrary a, Arbitrary b) => Gen (Either a b)
genEither = do
    x <- arbitrary
    y <- arbitrary
    elements [Left x, Right y]

{-type Morse = String

letterToMorse :: (M.Map Char Morse)
letterToMorse = M.fromList [
    ('a', ".-")
    , ('b', "-...")
    , ('c', "-.-.")
    , ('d', "-..")
    , ('e', ".")
    , ('f', "..-.")
    , ('g', "--.")
    , ('h', "....")
    , ('i', "..")
    , ('j', ".---")
    , ('k', "-.-")
    , ('l', ".-..")
    , ('m', "--")
    , ('n', "-.")
    , ('o', "---")
    , ('p', ".--.")
    , ('q', "--.-")
    , ('r', ".-.")
    , ('s', "...")
    , ('t', "-")
    , ('u', "..-")
    , ('v', "...-")
    , ('w', ".--")
    , ('x', "-..-")
    , ('y', "-.--")
    , ('z', "--..")
    , ('1', ".----")
    , ('2', "..---")
    , ('3', "...--")
    , ('4', "....-")
    , ('5', ".....")
    , ('6', "-....")
    , ('7', "--...")
    , ('8', "---..")
    , ('9', "----.")
    , ('0', "-----")
    ]

morseToLetter :: M.Map Morse Char
morseToLetter = M.foldrWithKey (flip. M.insert) M.empty letterToMorse

charToMorse :: Char -> Maybe Morse
charToMorse c = M.lookup c letterToMorse

stringToMorse :: String -> Maybe [Morse]
stringToMorse = mapM charToMorse

morseToChar :: Morse -> Maybe Char
morseToChar m = M.lookup m morseToLetter

convertToMorse :: IO ()
convertToMorse = forever $ do
    weAreDone <- isEOF
    when weAreDone exitSuccess
    line <- getLine
    convertLine line
    where
        convertLine line = do
            let morse = stringToMorse line
            case morse of
                (Just str) -> putStrLn $ intercalate " " str
                Nothing -> do
                    putStrLn $ "Error" ++ line
                    exitFailure

convertFromMorse :: IO ()
convertFromMorse = forever $ do
    weAreDone <- isEOF
    when weAreDone exitSuccess
    line <- getLine
    convertLine line
    where
        convertLine line = do
            let decoded :: Maybe String
                decoded = traverse morseToChar (words line)
            case decoded of
                (Just s) -> putStrLn s
                Nothing -> do
                    putStrLn $ "ERROR: " ++ line
                    exitFailure
-}

half :: Fractional a => a -> a
half x = x / 2

halfIdentity :: Double -> Bool
halfIdentity x= ((*2) . half) x == x

listOrdered :: (Ord a) => [a] -> Bool
listOrdered xs = snd $ foldr go (Nothing, True) xs
    where   go _ status@(_, False) = status
            go y (Nothing, t) = (Just y, t)
            go y (Just x, t) = (Just y, x >= y)

plusAssociative :: (Eq a, Num a) => a -> a -> a -> Bool
plusAssociative x y z =
    x + (y + z) == (x + y) + z
plusCommutative :: (Eq a, Num a) => a -> a -> Bool
plusCommutative x y =
    x + y == y + x

law1 :: Integral a => a -> a -> Bool
law1 _ 0 = True
law1 x y = (quot x y)*y + (rem x y) == x

law2 :: Integral a => a -> a -> Bool
law2 _ 0 = True
law2 x y = (div x y)*y + (mod x y) == x

law3 :: [Int] -> Bool
law3 x = (reverse.reverse) x== id x

genMy :: Gen Int
genMy = elements [1..10]

property1 :: Property
property1 = forAll genMy law1 

law4 x y= foldr (:) x y == (++) x y

square :: Num a => a -> a
square x = x * x
squareIdentity :: Double -> Double
squareIdentity = square . sqrt

genDouble :: Gen Double
genDouble = elements [0..100]
prop1 :: Double -> Bool
prop1 x = squareIdentity x ==  x
prop2 :: Property
prop2 = forAll genDouble prop1


--property2 :: Property
--property2 = forAll genF (\x -> f $ x -> x)