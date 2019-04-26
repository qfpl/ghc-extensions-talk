{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}

module GND where

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
