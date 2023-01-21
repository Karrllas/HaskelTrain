{-# LANGUAGE InstanceSigs #-}

module Monoid where

import Control.Monad
import Data.Monoid
import Test.QuickCheck
import Test.QuickCheck.Function
import Control.Applicative (Applicative(liftA2))

newtype First' a =
    First' { getFirst' :: Maybe a }
    deriving (Eq, Show)

instance (Arbitrary a) => Arbitrary (First' a) where 
     arbitrary :: Arbitrary a => Gen (First' a)
     arbitrary = fmap First' arbitrary


instance (Semigroup a) => Semigroup (First' a) where 
     First' Nothing <> a = a
     First' a <> First' b = First' (a <> b)


instance (Monoid a) => Monoid (First' a) where
    mempty = First' Nothing
    mappend (First' a) (First' Nothing) = First' (a<>Nothing)
    mappend (First' a) (First' b) = First' (a<>b)
    

firstMappend :: (Monoid a) => First' a -> First' a -> First' a
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

data Trivial = Trivial deriving (Eq, Show)
instance Semigroup Trivial where
    (<>) :: Trivial -> Trivial -> Trivial
    _ <> _ = Trivial
instance Arbitrary Trivial where
    arbitrary :: Gen Trivial
    arbitrary = return Trivial

semigroupAssoc :: (Eq m, Semigroup m) => m -> m -> m -> Bool
semigroupAssoc a b c = (a <> (b <> c)) == ((a <> b) <> c)

type TrivialAssoc = Trivial -> Trivial -> Trivial -> Bool

newtype Identity a = Identity a 
    deriving (Eq, Show)

instance Arbitrary a => Arbitrary (Identity a) where
    arbitrary :: Gen (Identity a)
    arbitrary = fmap Identity arbitrary

instance Semigroup a => Semigroup (Identity a) where 
    (Identity a) <> (Identity b) = Identity (a<>b)

type IdentityAssoc a = Identity a -> Identity a -> Identity a -> Bool

data Two a b = Two a b
    deriving (Eq, Show)

instance (Semigroup a, Semigroup b) => Semigroup (Two a b) where 
    (Two a b) <> (Two a' b') = Two (a<>a') (b<>b')

instance Functor (Two a) where 
    fmap f (Two a b) = Two a $ f b 

instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where 
    arbitrary :: (Arbitrary a, Arbitrary b) => Gen (Two a b)
    arbitrary = liftA2 Two arbitrary arbitrary 

type TwoAssoc a b = Two a b -> Two a b-> Two a b-> Bool

newtype BoolConj = BoolConj Bool deriving (Eq, Show)

instance Semigroup BoolConj where 
    BoolConj True <> BoolConj True = BoolConj True 
    _ <> _ = BoolConj False

instance Arbitrary BoolConj where 
    arbitrary :: Gen BoolConj
    arbitrary = fmap BoolConj arbitrary

type BoolAssocCon = BoolConj -> BoolConj -> BoolConj -> Bool
 
functorCompose' :: (Eq (f c), Functor f) =>
    f a
    -> Fun a b
    -> Fun b c
    -> Bool
functorCompose' x (Fun _ f) (Fun _ g) =
    (fmap (g . f) x) == (fmap g . fmap f $ x)

functorIdentity :: (Eq (f b), Functor f) => f b -> Bool
functorIdentity f =
    fmap id f == f


