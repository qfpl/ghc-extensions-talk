# Heavyweight {data-background="images/muhammad-ali.jpg"}

## `RecordWildCards`

::: {.left}
Elide fields from record construction and pattern matching.
:::

##

<pre class="haskell"><code data-trim data-noescape>
<span class="fragment" data-fragment-index="4">{-# LANGUAGE RecordWildCards #-}</span>

<span class="fragment fade-in-then-semi-out" data-fragment-index="1">data Person =
  Person {
    firstName :: Text
  , surname   :: Text
  , height    :: Integer
  }</span>

<span class="fragment fade-in-then-semi-out" data-fragment-index="2">greetPerson ::
  Person
  -> Text
greetPerson </span><span class="fragment" data-fragment-index="2"><span class="fragment highlight-red" data-fragment-index="3"><span class="fragment fade-out no-layout" data-fragment-index="5">Person{firstName = firstName, surname = surname, height = height}</span></span></span><span class="fragment no-layout" style="color: red" data-fragment-index="5">Person{..}</span><span class="fragment fade-in-then-semi-out" data-fragment-index="2"> =
  let
    heightDescriptor = bool "short" "tall" $ height &:gt; 180
  in
       "Hi, " <> firstName <> " " <> surname <> ". Aren't you "
    <> heightDescriptor <> "!"</span>
</code></pre>

##

<pre class="haskell"><code data-trim data-noescape>
{-# LANGUAGE RecordWildCards   #-}

<span class="fragment fade-in-then-semi-out" data-fragment-index="1">data ConferenceAttendee =
  ConferenceAttendee {
    confFirstName :: Text
  , confSurname   :: Text
  , confHeight    :: Integer
  , confShirtSize :: ShirtSize
  }</span>

<span class="fragment fade-in-then-semi-out" data-fragment-index="2">defaultConferenceAttendee ::
  Person
  -> ConferenceAttendee
defaultConferenceAttendee Person{..} =
  let
    confFirstName = firstName
    confSurname = surname
    confHeight = height</span>
    <span class="fragment" data-fragment-index="3">confShirtSize = M</span>
  <span class="fragment fade-in-then-semi-out" data-fragment-index="2">in</span>
    <span class="fragment" data-fragment-index="4">ConferenceAttendee {..}</span>
</code></pre>

##

<pre class="haskell"><code data-trim data-noescape>
<span class="fragment fade-in-then-semi-out" data-fragment-index="1">{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE RecordWildCards   #-}</span>

<span class="fragment fade-in-then-semi-out" data-fragment-index="2">data ConferenceAttendee =
  ConferenceAttendee {</span>
    <span class="fragment fade-in-then-semi-out" data-fragment-index="3">firstName :: Text
  , surname   :: Text
  , height    :: Integer
  , shirtSize :: ShirtSize</span>
  <span class="fragment fade-in-then-semi-out" data-fragment-index="2">}</span>

<span class="fragment fade-in-then-semi-out" data-fragment-index="4">defaultConferenceAttendee ::
  Person
  -> ConferenceAttendee
defaultConferenceAttendee</span> <span class="fragment" data-fragment-index="5">Person{..}</span> <span class="fragment fade-in-then-semi-out" data-fragment-index="4">=</span>
  <span class="fragment" data-fragment-index="6">ConferenceAttendee {shirtSize = M, ..}</span>
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
<span class="fragment fade-in-then-semi-out" data-fragment-index="1">defaultConferenceAttendee ::
  Person
  -> ConferenceAttendee
defaultConferenceAttendee</span> <span class="fragment" data-fragment-index="2">Person{firstName, surname, height}</span> <span class="fragment fade-in-then-semi-out" data-fragment-index="1">=</span>
  <span class="fragment fade-in-then-semi-out" data-fragment-index="1">ConferenceAttendee
  {</span> <span class="fragment fade-in-then-semi-out" data-fragment-index="3">confFirstName = firstName
  , confSurname = surname
  , confHeight = height
  , confShirtSize = M</span>
  <span class="fragment fade-in-then-semi-out" data-fragment-index="1">}</span>
</code></pre>

##

<pre class="haskell"><code data-trim data-noescape>
personName ::
  Person
  -> Text
personName Person{firstName, surname} =
  firstName <> " " <> surname
</code></pre>

##

::: {.left}
Improvements

 - Only bring into scope what we need.
 - Clear where fields come from.
:::

## `ScopedTypeVariables`

::: {.left}
Scope type variables to the lexical scope of their associated expressions.
:::

##

<pre class="haskell"><code data-trim data-noescape>
<span class="fragment" data-fragment-index="1">f ::
  [a] -> [a]</span>
<span class="fragment fade-in-then-semi-out" data-fragment-index="1">f xs =
  ys ++ ys
  where</span>
    <span class="fragment" data-fragment-index="1">ys :: [a]</span><span class="fragment" data-fragment-index="2"> </span>
    <span class="fragment fade-in-then-semi-out" data-fragment-index="1">ys = reverse xs</span>
</code></pre>

##

### TODO: highlighting/animation

<pre><code class="nohighlight" data-trim data-noescape>
Couldn't match type ‘a’ with ‘a1’
‘a’ is a rigid type variable bound by
  the type signature for:
    f :: forall a. [a] -> [a]
  at examples/ScopedTypeVariables.hs:(5,1)-(6,12)
‘a1’ is a rigid type variable bound by
  the type signature for:
    ys :: forall a1. [a1]
  at examples/ScopedTypeVariables.hs:10:5-13
Expected type: [a1]
  Actual type: [a]
</code></pre>

## `GeneralisedNewtypeDeriving`

::: {.left}
Derive instances for `newtype`s based on the type they wrap.
:::

##

<pre><code class="haskell" data-trim data-noescape>
class Pretty a where
  pretty :: a -> Text

instance Pretty Int where
  pretty = pack . show

newtype Age = Age Int
  deriving (Show, Pretty)
</code></pre>

##

<pre><code class="nohighlight" data-trim data-noescape>
Can't make a derived instance of ‘Pretty Age’:
  ‘Pretty’ is not a stock derivable class (Eq, Show, etc.)
  Try GeneralizedNewtypeDeriving for GHC's newtype-deriving extension
</code></pre>

##

<pre><code class="haskell" data-trim data-noescape>
<span class="fragment" data-fragment-index="2"><mark>{-# LANGUAGE GeneralisedNewtypeDeriving #-}</mark></span>

<span class="fragment fade-in-then-semi-out" data-fragment-index="1">class Pretty a where
  pretty :: a -> Text

instance Pretty Int where
  pretty = pack . show

newtype Age = Age Int
  deriving (Show, Pretty)</span>
</code></pre>

##

Something to be mindful of: GeneralisedNewtypeDeriving doesn't generate instances.

##

