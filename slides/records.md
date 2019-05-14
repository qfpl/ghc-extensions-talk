# Records

## `RecordWildCards`

::: {.left}
Elide fields from record construction and pattern matching.
:::

##

<pre class="haskell"><code data-trim data-noescape>
<span class="fragment" data-fragment-index="3"><mark>{-# LANGUAGE RecordWildCards #-}</mark></span>

data Person =
  Person {
    firstName :: Text
  , surname   :: Text
  , height    :: Integer
  }

<span class="fragment fade-in-then-semi-out" data-fragment-index="1">greetPerson ::
  Person
  -> Text
greetPerson </span><span class="fragment" data-fragment-index="1"><span class="fragment highlight-red" data-fragment-index="2"><span class="fragment fade-out no-layout" data-fragment-index="4">Person{firstName = firstName, surname = surname, height = height}</span></span></span><span class="fragment no-layout" style="color: red" data-fragment-index="4">Person{..}</span><span class="fragment fade-in-then-semi-out" data-fragment-index="1"> =
  undefined</span>
</code></pre>

##

<pre class="haskell"><code data-trim data-noescape>
{-# LANGUAGE RecordWildCards   #-}

defaultPerson ::
  Person
defaultPerson =
  let
    firstName = "Andrew"
    surname = "McMiddlin"
    height = 185
  in
    Person {..}
</code></pre>

##

<pre class="haskell"><code data-trim data-noescape>
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE RecordWildCards   #-}

<span class="fragment fade-in-then-semi-out" data-fragment-index="1">data ConferenceAttendee =
  ConferenceAttendee {
    firstName :: Text
  , surname   :: Text
  , height    :: Integer
  , shirtSize :: ShirtSize
  }</span>

<span class="fragment fade-in-then-semi-out" data-fragment-index="2">defaultConferenceAttendee ::
  Person
  -> ConferenceAttendee
defaultConferenceAttendee</span> <span class="fragment" data-fragment-index="3">Person{..}</span> <span class="fragment fade-in-then-semi-out" data-fragment-index="2">=</span>
  <span class="fragment" data-fragment-index="4">ConferenceAttendee {shirtSize = M, ..}</span>
</code></pre>

##

::: {.left}
Some problems with `RecordWildCards`

 - Unclear where variables come from.
 - All fields are brought into scope.
 - Vulnerable to changes in the record.
:::

::: {.notes}
- Especially if record definition not in your code base.
- Best to avoid unless you want all fields.
- Added fields are brought into scope --- get a warning if shadowing at least.
:::


## `NamedFieldPuns`

::: {.left}
Remove some of the boilerplate when bringing record fields into scope.
:::

##

<pre class="haskell"><code data-trim data-noescape>
{-# LANGUAGE NamedFieldPuns #-}

greetPerson ::
  Person
  -> Text
greetPerson <span class="fragment">Person{firstName, surname, height}</span> =
  undefined
</code></pre>

##

<pre class="haskell"><code data-trim data-noescape>
{-# LANGUAGE NamedFieldPuns #-}

greetPerson ::
  Person
  -> Text
greetPerson Person{firstName, surname} =
  undefined
</code></pre>

