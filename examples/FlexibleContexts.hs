{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}

module FlexibleContexts where

data Thing = Thing

data MyState = MyState Thing

updateThing ::
  MonadState MyState m
  => m ()
updateThing =
  undefined

class MonadReader r m | m -> r where
  ask :: m r

class MonadState s m | m -> s where
  get :: m s

-- Would normally do this with Lens
class HasThing s where
  getThing :: s -> Thing
  setThing :: Thing -> s

updateThing' ::
  ( HasThing s
  , MonadState s m
  )
  => m ()
updateThing' =
  undefined


