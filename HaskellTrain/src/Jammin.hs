{-# LANGUAGE StrictData #-}

module Jammin where

import Data.List hiding (isSubsequenceOf)
import GHC.Arr (accum)
import Control.Arrow (ArrowChoice(right))


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