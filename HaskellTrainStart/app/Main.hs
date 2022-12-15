module Main where


import System.IO
import Hello
main :: IO ()
main = do 
    c <- getChar
    c' <- getChar
    if c == 'a'
        then putStrLn "True"
        else return ()