{-# LANGUAGE MultiParamTypeClasses #-}

module MPTC where

class (Eq a) => Crumbs a where
  -- MultiParamTypeClasses allows this constraint to be added, which introduces a new type variable
  -- that shadows the one in the instance head.
  crumbs :: Show a => a -> String

data Blah = Blah deriving (Eq)

instance Crumbs Blah where
  crumbs = show
