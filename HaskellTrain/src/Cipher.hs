module Cipher where


import Data.Char
import Data.List.NonEmpty (xor)



caesar :: [Char] -> [Char]
caesar [] = []
caesar (x:xs) = if ord x `elem` [97..122] then go x : caesar xs else x : caesar xs
    where go :: Char -> Char 
          go x = if ord x > 118 then chr(ord x - 22) else chr(ord x + 4)

        

unCaesar :: String -> String
unCaesar [] = []
unCaesar (x:xs) = if ord x `elem` [97..122] then go x : unCaesar xs else x : unCaesar xs
    where go :: Char -> Char 
          go x = if ord x < 101 then chr(ord x + 22) else chr(ord x - 4)


