module GNDTests where

import Test.QuickCheck (Arbitrary (arbitrary), property, oneof)
import Test.QuickCheck.Checkers (quickBatch, EqProp (..))
import Test.QuickCheck.Classes (applicative, monad)

import GND

instance Arbitrary a => Arbitrary (Maybe' a) where
  arbitrary = oneof [
      Just' <$> arbitrary
    , Just'' . IdentityT . Just <$> arbitrary <*> arbitrary
    , pure Nothing'
    ]

instance Eq a => EqProp (Maybe' a) where
  Nothing' =-= Nothing' = property True
  Just' x =-= Just' y = property $ x == y
  Just'' ia1 n1 =-= Just'' ia2 n2 = property (ia1 == ia2 && n1 == n2)
  _ =-= _ = property False


main = do
  quickBatch $ applicative (Just' ((), 42 :: Int, "hi"))
  quickBatch $ monad (Just' ((), 42 :: Int, "hi"))