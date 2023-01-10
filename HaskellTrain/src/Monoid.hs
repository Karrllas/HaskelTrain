
module Monoid where

import Control.Monad
import Data.Monoid
import Test.QuickCheck

newtype First' a =
    First' { getFirst' :: Maybe a }
    deriving (Eq, Show)


instance Semigroup (First' a) where 
     First' Nothing <> a = a
     a <> _ = a 


instance Monoid (First' a) where
    mempty = undefined
    mappend = undefined

firstMappend :: First' a -> First' a -> First' a
firstMappend = mappend

type FirstMappend =
       First' String
    -> First' String
    -> First' String
    -> Bool

monoidLeftIdentity :: (Eq m, Monoid m) => m -> Bool
monoidLeftIdentity a = (mempty <> a) == a
monoidRightIdentity :: (Eq m, Monoid m) => m -> Bool
monoidRightIdentity a = (a <> mempty) == a
monoidAssoc :: (Eq m, Monoid m) => m -> m -> m -> Bool
monoidAssoc a b c = (a <> (b <> c)) == ((a <> b) <> c)

main :: IO ()
main = do
    quickCheck (monoidAssoc :: FirstMappend)
    quickCheck (monoidLeftIdentity :: First' String -> Bool)
    quickCheck (monoidRightIdentity :: First' String -> Bool)

 