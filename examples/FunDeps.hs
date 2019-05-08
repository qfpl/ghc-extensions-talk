{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}

module FunDeps where

class Monad m => MonadReader r m | m -> r  where
  ask :: m r

-- class Monad m => MonadReader r m where
--   ask :: m r

instance MonadReader r ((->) r) where
  ask = id

-- newtype Sinteger = Sinteger Integer
--   deriving (Eq, Show, Num, Ord, Real, Enum, Integral)

-- instance MonadReader Integer ((->) Sinteger) where
--   ask (Sinteger n) = n + 1

foo ::
  Integer
foo =
  (+ 1) <$> ask $ 41

bar ::
  MonadReader Integer m
  => m Integer
bar = (+1) <$> ask

class (Eq a) => Crumbs a b where
  crumbs :: 