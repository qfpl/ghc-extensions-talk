# WIP

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
  ask :: m r</span><span class="fragment" data-fragment-index="3"></span>
</code></pre>

<!-- ## -->

<!-- <pre class="no-highlight"><code data-trim data-noescape> -->
<!-- type-class-extensions.lhs:275:20-22: error: -->
<!--     • Ambiguous type variable ‘t0’ arising from the literal ‘100’ -->
<!--       prevents the constraint ‘(Num t0)’ from being solved. -->
<!--       Probable fix: use a type annotation to specify what ‘t0’ should be. -->
<!--       These potential instances exist: -->
<!--         instance Num Integer -- Defined in ‘GHC.Num’ -->
<!--         instance Num Double -- Defined in ‘GHC.Float’ -->
<!--         instance Num Float -- Defined in ‘GHC.Float’ -->
<!--         ...plus two others -->
<!--         (use -fprint-potential-instances to see them all) -->
<!--     • In the second argument of ‘($)’, namely ‘100’ -->
<!--       In the expression: (+ 1) <$> ask' $ 100 -->
<!--       In an equation for ‘foo'’: foo' = (+ 1) <$> ask' $ 100 -->
<!--     | -->
<!-- 275 |   (+ 1) <$> ask' $ 100 -->
<!--     |                    ^^^ -->
<!-- </code></pre> -->

