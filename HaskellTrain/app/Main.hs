module Main where

import Lib () 

playGame :: Strategy -> [Move] -> IO ()
playGame strategy moves = do { putStr "Enter Move: ";
                               inp <- getLine ;
                               putStrLn $ "AI plays: " ++ show (strategy moves);
                               case inp of 
                                   "Rock" -> playGame strategy (Rock:moves)
                                   "Paper" -> playGame strategy (Paper:moves)
                                   "Scissors" -> playGame strategy (Scissors:moves)
                                   _ -> return () } 

main :: IO ()
main = do playGame alwaysRock [] 


---- Rock  paper sci
--- Program Model

data Move = Rock | Paper | Scissors
    deriving (Show,Eq)

type Strategy = [Move] -> Move

--- Strategies

copyCat :: Strategy
copyCat [] = Rock
copyCat (latest:_) = latest

cycleS :: Strategy
cycleS moves = case length moves `mod` 3 of
                0 ->  Rock
                1 -> Paper
                2 -> Scissors
                _ -> Rock 

alwaysRock :: Strategy
alwaysRock _ = Rock