{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module MPTC where

import Data.Functor.Compose (Compose)

class (Eq a) => Crumbs a where
  -- MultiParamTypeClasses allows this constraint to be added, which introduces a new type variable
  -- that shadows the one in the instance head.
  crumbs :: Show a => a -> String

data Blah = Blah deriving (Eq)

instance Crumbs Blah where
  crumbs = show

-- TODO: How does instance selection for crumbs work?!


class (Functor t, Foldable t) => Traversable' t where
  traverse' :: Applicative f => (a -> f b) -> t a -> f (t b)

instance (Traversable' f, Traversable' g) => Traversable' (Compose f g) where
  traverse' :: (a -> h b) -> Compose f g a -> h (Compose f g b)
  traverse' = undefined

instance (Eq a, Eq b) => Eq ((->) a b) where
  f == g = undefined