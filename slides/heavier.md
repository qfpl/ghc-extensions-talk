# Heavyweight {data-background="images/muhammad-ali.jpg"}

## `RecordWildCards`

::: {.left}
Elide fields from record construction and pattern matching.
:::

##

<pre class="haskell"><code data-trim data-noescape>
<span class="fragment" style="color: red" data-fragment-index="4">{-# LANGUAGE RecordWildCards #-}</span>

<span class="fragment fade-in-then-semi-out" data-fragment-index="1">data Person =
  Person {
    firstName :: Text
  , surname   :: Text
  , height    :: Integer
  }</span>

<span class="fragment" data-fragment-index="2">greetPerson ::
  Person
  -> Text
greetPerson <span class="fragment highlight-red" data-fragment-index="3"><span class="fragment fade-out no-layout" data-fragment-index="5">Person{firstName = firstName, surname = surname, height = height}</span></span><span class="fragment no-layout" style="color: red" data-fragment-index="5">Person{..}</span> =
  let
    heightDescriptor = bool "short" "tall" $ height > 180
  in
       "Hi, " <> firstName <> " " <> surname <> ". Aren't you "
    <> heightDescriptor <> "!"</span>
</code></pre>

##

<pre class="haskell"><code data-trim data-noescape>
{-# LANGUAGE RecordWildCards #-}

data Person =
  Person {
    firstName :: Text
  , surname   :: Text
  , height    :: Integer
  }


greetPerson ::
  Person
  -> Text
greetPerson <span style="color: red">Person{..}</span> =
  let
    heightDescriptor = bool "short" "tall" $ height > 180
  in
       "Hi, " <> firstName <> " " <> surname <> ". Aren't you "
    <> heightDescriptor <> "!"
</code></pre>

## `NamedFieldPuns`

## `ScopedTypeVariables`

## `GeneralizedNewtypeDeriving`


