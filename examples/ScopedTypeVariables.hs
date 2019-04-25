-- {-# LANGUAGE ScopedTypeVariables #-}

module ScopedTypeVariables where

f ::
  [a] -> [a]
f xs =
  ys ++ ys
  where
    ys :: [a]
    ys = reverse xs
