{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Records where

import           Data.Bool (bool)
import           Data.Text (Text)
import qualified Data.Text as T

data Person =
  Person {
    firstName :: Text
  , surname   :: Text
  , height    :: Integer
  , age       :: Integer
  }

data ShirtSize =
  XS
  | S
  | M
  | L
  | XL
  | XXL
  | XXXL

data ConferenceAttendee =
  ConferenceAttendee {
    confFirstName :: Text
  , confSurname   :: Text
  , confHeight    :: Integer
  , confShirtSize :: ShirtSize
  }

foo :: Person -> Text
foo = firstName

personName ::
  Person
  -> Text
personName Person{firstName, surname} =
  firstName <> " " <> surname

defaultConferenceAttendee ::
  Person
  -> ConferenceAttendee
defaultConferenceAttendee Person{firstName, surname, height} =
  ConferenceAttendee
  { confFirstName = firstName
  , confSurname = surname
  , confHeight = height
  , confShirtSize = M
  }

defaultConferenceAttendee' ::
  Person
  -> ConferenceAttendee
defaultConferenceAttendee' =
  ConferenceAttendee
    <$> firstName
    <*> surname
    <*> height
    <*> pure XL

greetPerson ::
  Person
  -> Text
greetPerson Person{firstName = firstName, surname = surname, height = height} =
  let
    heightDescriptor = bool "short" "tall" $ height > 180
  in
    "Hi, " <> firstName <> " " <> surname <> ". Aren't you " <> heightDescriptor <> "!"

greetPerson' ::
  Person
  -> Text
greetPerson' Person{..} =
  let
    heightDescriptor = bool "short" "tall" $ height > 180
    age = 42

  in
    "Hi, " <> firstName <> " " <> surname <> ". Aren't you " <> heightDescriptor <> "!"
