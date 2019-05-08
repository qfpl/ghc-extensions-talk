# Type Classes

## Type classes in Haskell 2010

::: {.left}

::: {.fragment data-fragment-index="1"}
[Section 4.3.1 of the standard](https://www.haskell.org/onlinereport/haskell2010/haskellch4.html#x10-750004.3) covers type classes.
:::

:::{.fragment data-fragment-index="2"}
To summarise, it says that a type class declaration must have the following form.
:::
:::

<pre class="nohighlight fragment" style="font-size: 1.1em" data-fragment-index="3"><code data-trim data-noescape>
<span class="fragment highlight-current-green" data-fragment-index="4">class</span> <span class="fragment highlight-current-green" data-fragment-index="5">cx =></span> <span class="fragment highlight-current-green" data-fragment-index="6">C</span> <span class="fragment highlight-current-green" data-fragment-index="7">u</span> <span class="fragment highlight-current-green" data-fragment-index="8">where cdecls</span>
</code></pre>

::: {.left}
<ul>
<span class="fragment highlight-current-green" data-fragment-index="4"><span class="fragment" data-fragment-index="4"><li>_must_ have the `class` keyword;</li></span></span>
<span class="fragment highlight-current-green" data-fragment-index="5"><span class="fragment" data-fragment-index="5"><li>_may_ have a context;</li></span></span>
<span class="fragment highlight-current-green" data-fragment-index="6"><span class="fragment" data-fragment-index="6"><li>_must_ have a class name;</li></span></span>
<span class="fragment highlight-current-green" data-fragment-index="7"><span class="fragment" data-fragment-index="7"><li>_must_ be parameterised over exactly one type; and</li></span></span>
<span class="fragment highlight-current-green" data-fragment-index="8"><span class="fragment" data-fragment-index="8"><li>_may_ declare one or more members.</li></span></span>
</ul>
:::

##

::: {.left}
Some examples
:::

<pre class="haskell"><code data-trim data-noescape>
<span class="fragment fade-in-then-semi-out">class Eq a => Ord a where
  compare :: a -> a -> Ordering
  ...</span>

<span class="fragment fade-in-then-semi-out">class Show a where
  show :: a -> String
  ...</span>

<span class="fragment fade-in-then-semi-out">class (Ord a, Show a) => ShOrd a</span>
</code></pre>

::: {.notes}
`ShOrd` doesn't have a body, but is useful to express a set of constraints using one constraint.
:::

## Type class instances in Haskell 2010

::: {.left}
::: {.fragment data-fragment-index="1"}
[Section 4.3.2 of the standard](https://www.haskell.org/onlinereport/haskell2010/haskellch4.html#x10-750004.3)
covers type class instance declarations.
:::

::: {.fragment data-fragment-index="2"}
In short, it says that a type class instance must have the following form.
:::
:::

<pre class="fragment no-highlight" style="font-size: 1em" data-fragment-index="3"><code data-trim data-noescape>
<span class="fragment highlight-current-green" data-fragment-index="4">instance</span> <span class="fragment highlight-current-green" data-fragment-index="5">cx =></span> <span class="fragment highlight-current-green" data-fragment-index="6">C</span> <span class="fragment highlight-current-green" data-fragment-index="7">(T u1 … uk)</span> <span class="fragment highlight-current-green" data-fragment-index="8">where { d }</span>
</code></pre>

::: {.left}
<ul>
<span class="fragment" data-fragment-index="4"><span class="fragment highlight-current-green" data-fragment-index="4"><li>_must_ start with the `instance` keyword;</li></span></span>
<span class="fragment" data-fragment-index="5"><span class="fragment highlight-current-green" data-fragment-index="5"><li>_may_ have a context;</li></span></span>
<span class="fragment" data-fragment-index="6"><span class="fragment highlight-current-green" data-fragment-index="6"><li>_must_ mention the class name;</li></span></span>
<span class="fragment" data-fragment-index="7"><span class="fragment highlight-current-green" data-fragment-index="7"><li>_must_ mention the type the instance is for; and</li></span></span>
<span class="fragment" data-fragment-index="8"><span class="fragment highlight-current-green" data-fragment-index="8"><li>_may_ contain definitions for the class's members.</li></span></span>
</ul>
:::

## `MultiParamTypeClasses`

::: {.left}
Allows type classes with more than one type parameter.
:::

##

```{.haskell .fragment}
class Monad m => MonadReader r m where
  ask :: m r
  ...
```

::: {.fragment .left}
_must_ be parameterised over exactly one type
:::

::: {.notes}
- `MonadReader` is a common class from the `mtl` package.
- Allows us to avoid concrete transformer stacks throughout much of our code.
- Class is not permitted by Haskell 2010
:::

## `FlexibleInstances`

:::{.left}
Relaxes the rules for valid type class instances.
:::

##

<pre class="haskell"><code data-trim data-noescape>
<span class="fragment">class Monad m => MonadReader r m where
  ask :: m r</span>

<span class="fragment">instance MonadReader r ((->) r) where
  ask = id
</code></pre>

##

<pre class="no-highlight"><code data-trim data-noescape>
<span class="fragment fade-in-then-semi-out" data-fragment-index="1">type-class-extensions.lhs:123:10-32: error:
    • Illegal instance declaration for ‘MonadReader r ((->) r)’</span>
        <span class="fragment" data-fragment-index="1"><span class="fragment highlight-current-green" data-fragment-index="2">(All instance types must be of the form (T a1 ... an)
         where a1 ... an are *distinct type variables*</span></span><span class="fragment fade-in-then-semi-out" data-fragment-index="1">,
         and </span><span class="fragment" data-fragment-index="1"><span class="fragment highlight-current-green" data-fragment-index="3">each type variable appears at most once in the instance head.</span>
         <span class="fragment highlight-current-green" data-fragment-index="4">Use FlexibleInstances if you want to disable this.</span></span><span class="fragment fade-in-then-semi-out" data-fragment-index="1">)
    • In the instance declaration for ‘MonadReader r ((->) r)’
    |
123 | instance MonadReader r ((->) r) where</span>
</code></pre>

##

<pre class="haskell"><code data-trim data-noescape>
class Twizzle a where
  twizzle :: a -> Int

instance Twizzle (Maybe Integer) where
  twizzle = maybe 42 fromInteger
</code></pre>

::: {.notes}
**COME UP WITH A BETTER EXAMPLE**

`FlexibleInstances` also allows us to write instances for fully concrete types.
:::

## Not so benign

::: {.notes}
- Seen some people claim that FlexibleInstances is benign.
- Often is, but it can bite you if you're not careful.
:::

##

<pre class="haskell"><code data-trim data-noescape>
module FIA where

data A = A1 | A2 deriving (Eq, Ord, Show)

data Whoopsie a b c =
  Whoopsie a b c
  deriving (Eq, Show)
</code></pre>

##

<pre class="haskell"><code data-trim data-noescape>
<span class="fragment fade-in-then-semi-out" data-fragment-index="1">{-# LANGUAGE FlexibleInstances #-}</span>

<span class="fragment fade-in-then-semi-out" data-fragment-index="2">module FIB where

import Data.Set (Set, insert)
import FIA</span>

<span class="fragment fade-in-then-semi-out" data-fragment-index="3">data B = B deriving (Eq, Ord, Show)</span>

<span class="fragment fade-in-then-semi-out" data-fragment-index="4">instance Ord c => </span><span class="fragment" data-fragment-index="4">Ord (Whoopsie A B c)</span><span class="fragment fade-in-then-semi-out" data-fragment-index="4"> where
  compare (Whoopsie a1 b1 c1) (Whoopsie a2 b2 c2) =</span>
    <span class="fragment" data-fragment-index="4">compare a1 a2</span><span class="fragment fade-in-then-semi-out" data-fragment-index="4"> <> compare b1 b2 <> compare c1 c2</span><span class="fragment" data-fragment-index="5"></span>

<span class="fragment fade-in-then-semi-out" data-fragment-index="6">insB :: Ord c => Whoopsie A B c -> Set (Whoopsie A B c) -> Set (Whoopsie A B c)
insB = insert</span>
</code></pre>

##

<pre class="haskell"><code data-trim data-noescape>
<span class="fragment fade-in-then-semi-out" data-fragment-index="1">{-# LANGUAGE FlexibleInstances #-}

module FIC where

import Data.Set (Set, insert)
import FIA

data C = C deriving (Eq, Ord, Show)</span>

<span class="fragment fade-in-then-semi-out" data-fragment-index="2">instance Ord b => </span><span class="fragment" data-fragment-index="2">Ord (Whoopsie A b C)</span><span class="fragment fade-in-then-semi-out" data-fragment-index="2"> where
  compare (Whoopsie a1 b1 c1) (Whoopsie a2 b2 c2) =</span>
    <span class="fragment" data-fragment-index="2">compare a2 a1</span><span class="fragment fade-in-then-semi-out" data-fragment-index="2"> <> compare b1 b2 <> compare c1 c2</span><span class="fragment" data-fragment-index="3"></span>

<span class="fragment" data-fragment-index="4">insC :: Ord b => Whoopsie A b C -> Set (Whoopsie A b C) -> Set (Whoopsie A b C)
insC = insert</span>
</code></pre>

##

<pre class="haskell"><code data-trim data-noescape>
<span class="fragment fade-in-then-semi-out">module Main where

import Data.Set (Set, empty)

import FIA
import FIB
import FIC

test :: Set (Whoopsie A B C)
test =</span>
  <span class="fragment">insB (Whoopsie A1 B C) . </span><span class="fragment">insC (Whoopsie A1 B C) . </span><span class="fragment">insC (Whoopsie A2 B C) $ empty</span>

<span class="fragment">main :: IO ()
main =
  print test</span>
</code></pre>

##

<pre class="no-highlight"><code data-trim data-noescape>
<span class="fragment fade-in-then-semi-out">$ ghc --version
The Glorious Glasgow Haskell Compilation System, version 8.4.4</span>

<span class="fragment fade-in-then-semi-out">$ ghc -Wall -fforce-recomp Main.hs -o whoopsie</span>
<span class="fragment fade-in-then-semi-out">[1 of 4] Compiling FIA              ( FIA.hs, FIA.o )
[2 of 4] Compiling FIB              ( FIB.hs, FIB.o )
[3 of 4] Compiling FIC              ( FIC.hs, FIC.o )
[4 of 4] Compiling Main             ( Main.hs, Main.o )
Linking whoopsie ...</span>

<span class="fragment fade-in-then-semi-out">> ./whoopsie
fromList [Whoopsie A1 B C,Whoopsie A2 B C,Whoopsie A1 B C]</span>
</code></pre>

## `FunctionalDependencies`

::: {.left}
Express dependent relationships between type variables for type classes with multiple parameters.
:::

##

<pre class="haskell"><code data-trim data-noescape>
<span class="fragment fade-in-then-semi-out">class Monad m => MonadReader r m where
  ask :: m r</span>

<span class="fragment fade-in-then-semi-out">instance MonadReader r ((->) r) where
  ask = id</span>

<span class="fragment">foo ::
  Integer
foo =
  (+ 1) <$> ask $ 100</span>
</code></pre>

::: {.notes}
- Refresh memory after diversion.
- MPTC to declare our class.
- `FlexibleInstances` to define an instance for the function type.
:::

##

<pre class="no-highlight"><code data-trim data-noescape>
<span class="fragment fade-in-then-semi-out" data-fragment-index="1">type-class-extensions.lhs:275:13-16: error:
    • </span><span class="fragment" data-fragment-index="1">Ambiguous type variable ‘t0’ arising from a use of ‘ask’
      prevents the constraint ‘(MonadReader
                                  Integer ((->) t0))’ from being solved.</span>
      <span class="fragment fade-in-then-semi-out" data-fragment-index="1">Probable fix: use a type annotation to specify what ‘t0’ should be.
      These potential instance exist:
        one instance involving out-of-scope types
        (use -fprint-potential-instances to see them all)
    • In the second argument of ‘(<$>)’, namely ‘ask’
      In the expression: (+ 1) <$> ask
      In the expression: (+ 1) <$> ask $ 100</span>
    <span class="fragment" data-fragment-index="1">|
275 |   (+ 1) <$> ask $ 100
    |             ^^^</span><span class="fragment" data-fragment-index="2"></span>
</code></pre>

::: {.notes}
- Intuition might say that 100 must be an Integer --- only one instance of MonadReader.
- To make it clearer, what if I defined a new instance.
:::

##

<pre class="haskell"><code data-trim data-noescape>
<span class="fragment fade-in-then-semi-out">{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module FunDeps where</span>

<span class="fragment fade-in-then-semi-out">class Monad m => MonadReader r m where
  ask :: m r

instance MonadReader r ((->) r) where
  ask = id</span>

<span class="fragment fade-in-then-semi-out">newtype Sinteger = Sinteger Integer
  deriving (Eq, Show, Num, Ord, Real, Enum, Integral)</span>

<span class="fragment fade-in-then-semi-out">instance MonadReader Integer ((->) Sinteger) where
  ask (Sinteger n) = n + 1</span>

<span class="fragment">foo ::
  Integer
foo =
  (+ 1) <$> ask $ 41</span>
</code></pre>

##

<pre class="haskell"><code data-trim data-noescape>
<span class="fragment fade-in-then-semi-out" data-fragment-index="1">{-# LANGUAGE FunctionalDependencies #-}</span>

<span class="fragment fade-in-then-semi-out" data-fragment-index="2">class Monad m => MonadReader r m </span><span class="fragment" data-fragment-index="2">| m -> r </span><span class="fragment fade-in-then-semi-out" data-fragment-index="2">where
  ask :: m r</span></span><span class="fragment" data-fragment-index="3"></span>

<span class="fragment" data-fragment-index="4">instance MonadReader r ((->) r) where
  ask = id</span>

<span class="fragment" data-fragment-index="5">instance MonadReader Integer ((->) Sinteger) where
  ask (Sinteger n) = n + 1</span>
</code></pre>

##

<pre class="no-highlight"><code data-trim data-noescape>
FunDeps.hs:14:10: error:
    <span style="color: red">Functional dependencies conflict between instance declarations:
      instance MonadReader r ((->) r) -- Defined at FunDeps.hs:14:10
      instance MonadReader Integer ((->) Sinteger)</span>
        -- Defined at FunDeps.hs:20:10
   |
14 | instance MonadReader r ((->) r) where
   |          ^^^^^^^^^^^^^^^^^^^^^^
</code></pre>

##

<pre class="haskell"><code data-trim data-noescape>
<span class="fragment fade-in-then-semi-out" data-fragment-index="1">{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}</span>
<span class="fragment fade-in-then-semi-out" data-fragment-index="2"><mark>{-# LANGUAGE FunctionalDependencies #-}</mark></span>

<span class="fragment fade-in-then-semi-out" data-fragment-index="1">module FunDeps where</span>

<span class="fragment fade-in-then-semi-out" data-fragment-index="3">class Monad m => MonadReader r m</span> <span class="fragment" data-fragment-index="3">| m -> r </span><span class="fragment fade-in-then-semi-out" data-fragment-index="3">where
  ask :: m r</span><span class="fragment" data-fragment-index="4"></span>

<span class="fragment fade-in-then-semi-out" data-fragment-index="5">instance MonadReader r ((->) r) where
  ask = id</span>

<span class="fragment fade-in-then-out" data-fragment-index="6">newtype Sinteger = Sinteger Integer
  deriving (Eq, Show, Num, Ord, Real, Enum, Integral)

instance MonadReader Integer ((->) Sinteger) where
  ask (Sinteger n) = n + 1</span><span class="fragment" data-fragment-index=7"></span>

<span class="fragment fade-in-then-semi-out" data-fragment-index="8">foo ::
  Integer
foo =
  (+ 1) <$> ask $ 41</span>
</code></pre>

