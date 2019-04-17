{-# LANGUAGE MultiWayIf #-}

module MultiWayIf where

ifs =
  if 1 < 0 then
    "foo"
  else if 12 > 4 then
    "bar"
  else if even 42 then
    "42"
  else
    "no idea"

multi =
  if
    | 1 < 0 -> "foo"
    | 12 > 4 -> "bar"
    | even 42 -> "42"
    | otherwise -> "no idea"
