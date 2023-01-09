module Another where

import Control.Applicative
import System.Random 
import Control.Applicative (liftA3)
import Control.Monad (replicateM)
import Control.Monad.Trans.State



data Die =
    DieOne
    | DieTwo
    | DieThree
    | DieFour
    | DieFive
    | DieSix
    deriving (Eq, Show)

intToDie :: Int -> Die
intToDie n =
    case n of
    1 -> DieOne
    2 -> DieTwo
    3 -> DieThree
    4 -> DieFour
    5 -> DieFive
    6 -> DieSix

    x -> error $ "intToDie got non 1-6 integer: " ++ show x

rollDie3Tiems :: (Die,Die,Die)
rollDie3Tiems = do 
    let s = mkStdGen 12
        (d1,s1) = randomR (1,6) s
        (d2,s2) = randomR (1,6) s1
        (d3,_) = randomR (1,6) s2
    (intToDie d1, intToDie d2, intToDie d3)

rollDie :: State StdGen Die
rollDie = state $ do 
    (n,s) <- randomR (1,6)
    return (intToDie n , s)




rollDie' :: State StdGen Die
rollDie' =
    intToDie <$> state (randomR (1, 6))

rollDieThreeTimes' :: State StdGen (Die, Die, Die)
rollDieThreeTimes' = liftA3 (,,) rollDie rollDie rollDie

infiniteDie :: State StdGen [Die]
infiniteDie = repeat <$> rollDie

nDie :: Int -> State StdGen [Die]
nDie n = replicateM n rollDie



hurr :: (Num a) => a -> a
hurr = (*2)

durr :: (Num a) => a -> a
durr = (+10)

m :: (Num a) => a -> a
m = hurr.durr

m' :: (Num a) => a -> a
m' = fmap hurr durr

m2 :: Integer -> Integer
m2 = (+) <$> hurr <*> durr

m3 :: Integer -> Integer
m3 = liftA2 (+) hurr durr

