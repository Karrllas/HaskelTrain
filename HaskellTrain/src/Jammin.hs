{-# LANGUAGE StrictData #-}

module Jammin where


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