{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module FunDepsA where

import FunDeps

newtype Sinteger = Sinteger Integer deriving (Eq, Show, Num, Ord, Real, Enum, Integral)

instance MonadReader Integer ((->) Sinteger) where
  ask (Sinteger n) = n + 1
-- instance Integral r => MonadReader Float ((->) r) where
--   ask = fromIntegral

-- instance MonadReader Float ((->) Int) where
--   ask = fromIntegral

foo ::
  Integer
foo =
  (+ 1) <$> ask $ 41
