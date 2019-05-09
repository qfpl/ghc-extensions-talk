{-# LANGUAGE ScopedTypeVariables #-}

module ScopedTypeVariables where

f ::
  forall a.
  [a] -> [a]
f xs =
  ys ++ ys
  where
    ys :: [a]
    ys = reverse xs
