-- {-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module GND where

import Control.Applicative (Applicative)
import Data.Set
-- import Data.Text (Text, pack)

-- data family Z :: * -> *

-- newtype instance Z Int = ZI Double
-- newtype instance Z Moo = ZM (Int,Int)

-- type role IsInt nominal
-- class IsInt t where
--     isInt :: c Int -> c t

-- newtype Moo = Moo Int deriving (IsInt)

-- instance IsInt Int where isInt = id

-- main = case isInt (ZI 4.0) of ZM tu -> print tu

-- class Pretty a where
--   pretty :: a -> Text

-- instance Pretty Int where
--   pretty = pack . show

-- newtype Age = Age Int
--   deriving (Show, Pretty)

class Applicative m => Monad' m where
  join :: m (m a) -> m a

-- instance Monad' Maybe where
--   join Nothing = Nothing
--   join (Just ma) = ma

data Foo = Foo

data TF a where
  TMaybeInt :: Maybe Int -> TF Foo
  TFoo :: TF Foo -> TF (Either String Int)
  -- TF (IdentityT Maybe Int) = IO String

newtype IdentityT m a = IdentityT (m a)
  deriving (Functor, Applicative, Monad)

foo =
  let
    a = IdentityT (TMaybeInt (Just 12)) :: IdentityT TF Foo
    b = IdentityT (TFoo (IdentityT (TMaybeInt (Just 12)))) :: IdentityT TF (IdentityT TF Foo)
    --b = IdentityT (IdentityT (Just 12)) :: IdentityT TF (IdentityT TF Foo)
  in
    undefined

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
