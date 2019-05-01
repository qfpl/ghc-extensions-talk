# Heavyweight {data-background="images/muhammad-ali.jpg"}

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

::: {.left}
`GeneralisedNewtypeDeriving` as it was originally implemented had some issues that resulted in **roles** being added to the language.

- nominal
- representational
- phantom
:::

## Nominal equality

::: {.left}
Two types are nominally equal if they are the same type.
:::

<pre><code class="haskell" data-trim data-noescape>
<span class="fragment">Int ~ Int</span>
<span class="fragment">Maybe Text ~ Maybe Text</span>
<span class="fragment">forall a. [a] ~ [a]</span>
</code></pre>

::: {.notes}
- Mostly what we think of when we think about equality of types in Haskell.
- Used by `~` constraint.
:::

## Representational equality

::: {.left}
Two types that have the same machine representation are equal.

<span class="fragment">`Coercible` constraint in Haskell denotes representational equality.</span>
:::

<pre><code class="haskell" data-trim data-noescape>
<span class="fragment">class Coercible a b</span>

<span class="fragment">coerce :: Coercible a b => a -> b</span>
</code></pre>

##

<pre><code class="haskell" data-trim data-noescape>
<span class="fragment">newtype Age = Age Int</span>

<span class="fragment">Coercible Age Int
Coercible Int Age</span>

<span class="fragment">Coercible (Maybe Int) (Maybe Age)
Coercible [Age] [Int]</span>

<span class="fragment">Coercible (Int -> Int) (Age -> Age)</span>
</code></pre>

##

##

<pre><code class="haskell" data-trim data-noescape>
<span class="fragment fade-in-then-semi-out" data-fragment-index="1">class Pretty a where
  pretty :: a -> Text

instance Pretty Int where
  pretty = pack . show

newtype Age = Age Int
  deriving (Show)</span>
  
<span class="fragment fade-in-then-semi-out" data-fragment-index="2">instance Pretty Age where
  pretty = </span><span class="fragment" data-fragment-index="2">coerce</span><span class="fragment fade-in-then-semi-out" data-fragment-index="2"> (pack . show)</span><span class="fragment" data-fragment-index="3"></span>
</code></pre>

::: {.notes}
 - Before roles, newtypes could cause seg faults in some special circumstances.
 - Focusing on current implementation that fixes that problem.
:::

##

```haskell
coerce :: Coercible a b => a -> b

class Coercible a b

instance Coercible a a

newtype Age = Age Int

instance Coercible a Int => Coercible a Age
instance Coercible Int b => Coercible Age b
```

::: {.notes}
- Coercible instances tell the type checker that two types have the same representation.
   + same bit pattern in memory
- Programmers cannot create instances of `Coercible`.
- This is just a view that is familiar to programmers.
:::

##

The roles that bind us.

##

<pre><code class="haskell" data-trim data-noescape>
<span class="fragment fade-in-then-semi-out" data-fragment-index="1">newtype IdentityT m a = IdentityT (m a)
  deriving (Functor, Applicative, </span><span class="fragment" data-fragment-index="1">Monad</span><span class="fragment fade-in-then-semi-out" data-fragment-index="1">)</span><span class="fragment" data-fragment-index="2"></span>
</code></pre>

##

<pre><code class="haskell" data-trim data-noescape>
<span class="fragment fade-in-then-semi-out" data-fragment-index="1">class Applicative m => MonadJoin m where
  join :: m (m a) -> m a</span>

<span class="fragment fade-in-then-semi-out" data-fragment-index="2">newtype IdentityT m a = IdentityT (m a)
  deriving (Functor, Applicative, </span><span class="fragment" data-fragment-index="2">MonadJoin</span><span class="fragment fade-in-then-semi-out" data-fragment-index="2">)</span><span class="fragment" data-fragment-index="3"></span>
</code></pre>

##

```
    • Couldn't match representation of type ‘m (IdentityT m a)’
                               with that of ‘m (m a)’
        arising from the coercion of the method ‘join’
          from type ‘forall a. m (m a) -> m a’
            to type ‘forall a. IdentityT m (IdentityT m a) -> 
IdentityT m a’
      NB: We cannot know what roles the parameters to ‘m’ have;
        we must assume that the role is nominal
    • When deriving the instance for (Monad' (IdentityT m))
   |
43 |   deriving (Functor, Applicative, Monad')
   |                                   ^^^^^^
```

##

```haskell
class Monad' m where
  join :: m (m a) -> m a

newtype IdentityT m a = IdentityT (m a)
  deriving (Functor, Applicative)
  
instance Monad' m => Monad' (IdentityT m) where
  join :: IdentityT m (IdentityT m a) -> IdentityT m a
  join = coerce (join :: m (m a) -> m a)

-- have
instance Coercible (m a) b => Coercible (IdentityT m a) b
instance Coercible b (m a) => Coercible b (IdentityT m a)

-- need
instance Coercible (m (m a) -> m a) (IdentityT m (IdentityT m a) -> IdentityT m a)

That implies the following constraints (required by Coercible instance for functions?):
  Coercible (m a) (Identity m a)
  Coercible (m (m a)) (IdentityT m (IdentityT m a))
  
First one trivial given newtype wrapper.

Second one fails. Can get to `Coercible (m (m a)) (m (IdentityT m a))` by unwrapping the outer IdentityT.
However, `m` is unconstrained and could be a type family or something such that its parameters must be
nominally equal for equality to hold. We have no witness for this!

SIDE NOTE: quantified constraints allow us to constrain instances such that `m` has to have representational
type parameters.

```

::: {.notes}

:::
