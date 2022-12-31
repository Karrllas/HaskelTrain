{-# LANGUAGE StrictData #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleInstances #-}

module Jammin where

import Data.List hiding (isSubsequenceOf)
import GHC.Arr (accum, STArray)
import Control.Arrow (ArrowChoice(right))
import Control.Monad (join)


data Fruit =
     Peach
    |Plum
    |Apple
    |Blackberry
    deriving (Eq,Ord,Show)

data JamJars =
    Jam {fruit :: Fruit,
         jars  :: Int}
    deriving (Eq,Ord,Show)


row1 :: [JamJars]
row1 = [Jam{fruit = Apple , jars = 10}, Jam{fruit = Peach , jars = 5}, Jam{fruit = Blackberry , jars = 7}]
row2 :: [JamJars]
row2 = [Jam{fruit = Apple , jars = 2}, Jam{fruit = Plum , jars = 1}, Jam{fruit = Peach , jars = 8}]
row3 :: [JamJars]
row3 = [Jam{fruit = Peach , jars = 8}]
row4 :: [JamJars]
row4 = [Jam{fruit = Blackberry  , jars = 5}, Jam{fruit = Apple , jars = 11}, Jam{fruit = Plum , jars = 1}]
row5 :: [JamJars]
row5 = [Jam{fruit = Plum , jars = 22}]
row6 :: [JamJars]
row6 = [Jam{fruit = Apple , jars = 10}, Jam{fruit = Blackberry  , jars = 10}]
rows = row1 ++ row2 ++ row3 ++ row4 ++ row5 ++ row6

total :: [JamJars] -> Int
total  = foldr ((+).jars) 0

compareKind :: JamJars -> JamJars -> Ordering
compareKind (Jam k _) (Jam l _) = compare k l

sortedJams :: [JamJars] -> [JamJars]
sortedJams = sortBy compareKind

groupKind :: JamJars -> JamJars -> Bool
groupKind (Jam k _) (Jam l _) = k == l

groupedJams :: [JamJars] -> [[JamJars]]
groupedJams = groupBy groupKind

data OperatingSystem =
    GnuPlusLinux
    | OpenBSDPlusNevermindJustBSDStill
    | Mac
    | Windows
    deriving (Eq, Show)
data ProgrammingLanguage =
    Haskell
    | Agda
    | Idris
    | PureScript
    deriving (Eq, Show)
data Programmer =
    Programmeeer { os :: OperatingSystem
               , lang :: ProgrammingLanguage }
    deriving (Eq, Show)

allOperatingSystems :: [OperatingSystem]
allOperatingSystems =
    [ GnuPlusLinux
    , OpenBSDPlusNevermindJustBSDStill
    , Mac
    , Windows
    ]

allLanguages :: [ProgrammingLanguage]
allLanguages = [Haskell, Agda, Idris, PureScript]

allProgrammers :: [OperatingSystem] -> [ProgrammingLanguage] -> [Programmer]
allProgrammers xs ys = [Programmeeer x y | x <- xs , y <- ys ]

data Product a b =
    a :&: b
    deriving (Eq, Show)

    --- Trees

data BinaryTree a =
        Leaf
        | Node (BinaryTree a) a (BinaryTree a)
        deriving (Eq, Ord, Show)


insert' :: Ord a => a -> BinaryTree a -> BinaryTree a
insert' b Leaf = Node Leaf b Leaf
insert' b (Node left a right)
    | b == a = Node left a right
    | b < a = Node (insert' b left) a right
    | b > a = Node left a (insert' b right)
    | otherwise = Node Leaf b Leaf


mapTree :: (a -> b) -> BinaryTree a -> BinaryTree b
--mapTree f (Node Leaf a Leaf) = Node Leaf (f a) Leaf
mapTree _ Leaf = Leaf
mapTree f (Node left a right) = Node (mapTree f left) (f a) (mapTree f right)


preorder :: BinaryTree a -> [a]
preorder Leaf = []
preorder (Node left a right) = a : preorder left ++ preorder right
inorder :: BinaryTree a -> [a]
inorder Leaf = []
inorder (Node left a right) = inorder left ++ a:inorder right
postorder :: BinaryTree a -> [a]
postorder Leaf = []
postorder (Node left a right) = postorder left ++ postorder right ++ [a]
testTree :: BinaryTree Integer
testTree = Node (Node Leaf 1 Leaf) 2 (Node Leaf 3 Leaf)
testPreorder :: IO ()
testPreorder =
    if preorder testTree == [2, 1, 3]
    then putStrLn "Preorder fine!"
    else putStrLn "Bad news bears."
testInorder :: IO ()
testInorder =
    if inorder testTree == [1, 2, 3]
    then putStrLn "Inorder fine!"
    else putStrLn "Bad news bears."
testPostorder :: IO ()
testPostorder =
    if postorder testTree == [1, 3, 2]
    then putStrLn "Postorder fine!"
    else putStrLn "postorder failed check"
{-main :: IO ()
main = do
testPreorder
testInorder
testPostorder-}

{-foldrTree :: (a -> b -> b) -> b -> BinaryTree a -> b
foldrTree f acc Leaf = acc
foldrTree f acc (Node Leaf a Leaf ) =  f a acc
foldrTree f acc (Node left a Leaf ) =  f a (foldrTree f acc left)
foldrTree f acc (Node Leaf a right) =  f a (foldrTree f acc right)
foldrTree f acc (Node left a right) =  (foldrTree f acc left) a (foldrTree f acc right) ?????????????????????????????????????????????????????????????????????????-}

isSubsequenceOf :: (Eq a) => [a] -> [a] -> Bool
isSubsequenceOf xs ys = foldr (\ x -> (&&) (elem x ys)) True xs


myIterate :: (a -> a) -> a -> [a]
myIterate f a = a : myIterate f (f a)

myUnfoldr :: (b -> Maybe (a, b)) -> b -> [a]
myUnfoldr f x = case f x of
    Nothing -> []
    Just (y,z) -> y : myUnfoldr f z


{-data Optional a =
    Nada
    | Only a
    deriving (Eq, Show)

instance Monoid a => Monoid (Optional a) where
mempty = Nada
mappend Nada Nada  = Nada
mappend Nada (Only x) = Only x
mappend (Only x) Nada = Only x
mappend (Only x) (Only y) = Only (x > y) -}
-- Prelude> :t (fmap . fmap) replaceWithP
-- (fmap . fmap) replaceWithP
-- :: (Functor f1, Functor f) => f (f1 a) -> f (f1 Char)

replaceWithP :: b -> Char
replaceWithP = const 'p'
lms :: [Maybe [Char]]
lms = [Just "Ave", Nothing, Just "woohoo"]


replaceWithP' :: [Maybe [Char]] -> Char
replaceWithP' = replaceWithP


liftedReplace :: Functor f => f a -> f Char
liftedReplace = fmap replaceWithP


liftedReplace' :: [Maybe [Char]] -> [Char]
liftedReplace' = liftedReplace


twiceLifted :: (Functor f1, Functor f) =>
    f (f1 a) -> f (f1 Char)
twiceLifted = (fmap . fmap) replaceWithP

twiceLifted' :: [Maybe [Char]] -> [Maybe Char]
twiceLifted' = twiceLifted


thriceLifted :: (Functor f2, Functor f1, Functor f) =>
    f (f1 (f2 a)) -> f (f1 (f2 Char))
thriceLifted = (fmap . fmap . fmap) replaceWithP
-- More specific or "concrete"
thriceLifted' :: [Maybe [Char]] -> [Maybe [Char]]
thriceLifted' = thriceLifted

{-main :: IO ()
main = do
putStr "replaceWithP' lms: "
print (replaceWithP' lms)
putStr "liftedReplace lms: "
print (liftedReplace lms)
putStr "liftedReplace' lms: "
print (liftedReplace' lms)
putStr "twiceLifted lms: "
print (twiceLifted lms)
putStr "twiceLifted' lms: "
print (twiceLifted' lms)
putStr "thriceLifted lms: "
print (thriceLifted lms)
putStr "thriceLifted' lms: "
print (thriceLifted' lms)-}

newtype Constant a b =
    Constant { getConstant :: a }
    deriving (Eq, Show)

instance Functor (Constant m) where
    fmap _ (Constant v) = Constant v

type Nat f g = forall a . f a -> g a

maybeToList :: Nat Maybe []
maybeToList Nothing = []
maybeToList (Just a) = []

data Tuple a b =
    Tuples a b
    deriving (Eq, Show)

newtype Flip f a b =
    Flips (f b a)
    deriving (Eq, Show)

instance Functor (Flip Tuple a) where
    fmap f (Flips (Tuples a b)) = Flips  (Tuples (f a) b)


validateLength :: Int -> String -> Maybe String
validateLength maxlen s =
    if length s > maxlen
    then Nothing
    else Just s

newtype Name = Name String deriving (Eq, Show )
newtype Address = Address String deriving (Eq, Show)

mkName :: String -> Maybe Name
mkName s = Name <$> validateLength 25 s

mkAddress :: String -> Maybe Address
mkAddress s = Address <$> validateLength 100 s

data Person =
    Person Name Address deriving (Eq, Show)

mkPerson :: String -> String -> Maybe Person
mkPerson n a = if mkName n == Nothing || mkAddress a == Nothing
               then Nothing
               else Just (Person (Name  n)  (Address a))

mkPerson' :: String -> String -> Maybe Person
mkPerson' n a = Person <$> mkName n <*> mkAddress a

bind :: Monad m => (a -> m b) -> m a -> m b
bind f x = join $ fmap f x

twiceWhenEven :: [Integer] -> [Integer]
twiceWhenEven xs = do
    x <- xs
    if even x
    then [x*x, x*x]
    else []

twiceWhenEven' :: [Integer] -> [Integer]
twiceWhenEven' xs = join $ fmap (\x -> if even x then [x,x] else []) xs

twiceWhenEven'' :: [Integer] -> [Integer]
twiceWhenEven'' xs = xs >>= \x -> if even x then [x,x] else []


f :: Maybe Integer
f = Nothing
g :: Maybe String
g = Just "1"
h :: Maybe Integer
h = Just 10191
zed :: a -> b -> c -> (a, b, c)
zed = (,,)
doSomething = do
    a <- f
    b <- g
    c <- h
    return (zed a b c)
zed' :: Monad m => a -> b -> c -> m (a, b, c)
zed' a b c = return (a, b, c)
doSomething' = do
    a <- f
    b <- g
    c <- h
    zed' a b c

    newtype Constant a b =
        Constant { getConstant :: a }
        deriving (Eq, Ord, Show)
instance Functor (Constant a) where
    fmap f (Constant a)= Constant a
instance Monoid a => Applicative (Constant a) where
    pure a = Constant mempty
    (<*>) (Constant f) (Constant c) = Constant (f `mappend` c)
