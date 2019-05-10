-- {-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}

module GND where

import Control.Applicative (Applicative)
import Data.Set
import Data.Text (Text, pack)

-- data family Z :: * -> *

-- newtype instance Z Int = ZI Double
-- newtype instance Z Moo = ZM (Int,Int)

-- type role IsInt nominal
-- class IsInt t where
--     isInt :: c Int -> c t

-- newtype Moo = Moo Int deriving (IsInt)

-- instance IsInt Int where isInt = id

-- main = case isInt (ZI 4.0) of ZM tu -> print tu

class Pretty a where
  pretty :: a -> Text

instance Pretty Int where
  pretty = pack . show

newtype Age = Age Int
  deriving (Show, Pretty)

class Applicative m => Monad' m where
  join :: m (m a) -> m a

-- instance Monad' Maybe where
--   join Nothing = Nothing
--   join (Just ma) = ma

data Foo = Foo

data Maybe' a where
  Nothing' :: Maybe' a
  Just' :: a -> Maybe' a
  Just'' :: IdentityT Maybe a -> Int -> Maybe' a
  -- TF (IdentityT Maybe Int) = IO String

deriving instance Show a => Show (Maybe' a)

instance Functor Maybe' where
  fmap f Nothing' = Nothing'
  fmap f (Just' a) = Just' (f a)
  fmap f (Just'' (IdentityT ma) n) = Just'' (IdentityT (fmap f ma)) n

instance Applicative Maybe' where
  pure = Just'

  Nothing' <*> a = Nothing'

  Just' _ <*> Nothing' = Nothing'
  Just' f <*> Just' a = Just' $ f a
  Just' f <*> Just'' ia n = Just'' (f <$> ia) n

  Just'' _ _ <*> Nothing' = Nothing'
  Just'' (IdentityT mf) n <*> (Just' a) = Just'' (IdentityT (mf <*> Just a)) n
  Just'' imf n <*> Just'' ima n2 = Just'' (imf <*> ima) (n + n2)


newtype IdentityT m a = IdentityT (m a)
  deriving (Eq, Functor, Applicative, Monad, Show)

-- instance Monad' (IdentityT m) where
--   join = coerce (join :: m (m a) -> m a)


--------------------------------------------
-- FROM TRAC BUG https://gitlab.haskell.org/ghc/ghc/issues/1496
-- I DON'T THINK THIS IS RELEVANT OR IMPORTANT
--------------------------------------------
-- class IsoInt a where
--     convFromInt :: item Int -> item a

-- instance IsoInt Int where
--     convFromInt = id

-- newtype Down a = Down a deriving (Eq, Show, IsoInt)

-- -- deriving instance IsoInt a => IsoInt (Down a)

-- instance Ord a => Ord (Down a) where
--     compare (Down a) (Down b) = compare b a

-- asSetDown :: Set (Down Int) -> Set (Down Int)
-- asSetDown = id

-- a1 = toAscList . asSetDown . convFromInt . fromAscList $  [0..10]
-- a2 = toAscList . asSetDown . fromAscList . reverse . convFromInt $ [0..10]

-- main = do
--     print a1
--     print a2

type family Fam a where
  Fam Int = Bool
  Fam Age = String

data Quux a b c =
  Quux a b c