# WIP

## `FunctionalDependencies`

##

<pre class="haskell"><code data-trim data-noescape>
<span class="fragment fade-in-then-semi-out">class Monad m => MonadReader r m where
  ask :: m r</span>
  
<span class="fragment fade-in-then-semi-out">instance MonadReader r ((->) r) where
  ask = id</span>
</code></pre>

::: {.notes}
- Refresh memory after diversion.
- MPTC to declare our class.
- `FlexibleInstances` to define an instance for the function type.
:::

##

<pre class="haskell"><code data-trim data-noescape>
foo ::
  Integer
foo =
  (+ 1) <$> ask' $ 100
</code></pre>

##

<pre class="no-highlight"><code data-trim data-noescape>
type-class-extensions.lhs:275:13-16: error:
    • Ambiguous type variable ‘t0’ arising from a use of ‘ask'’
      prevents the constraint ‘(MonadReader'
                                  Integer ((->) t0))’ from being solved.
      Probable fix: use a type annotation to specify what ‘t0’ should be.
      These potential instance exist:
        one instance involving out-of-scope types
        (use -fprint-potential-instances to see them all)
    • In the second argument of ‘(<$>)’, namely ‘ask'’
      In the expression: (+ 1) <$> ask'
      In the expression: (+ 1) <$> ask' $ 100
    |
275 |   (+ 1) <$> ask' $ 100
    |             ^^^^
</code></pre>

##

<pre class="no-highlight"><code data-trim data-noescape>
type-class-extensions.lhs:275:20-22: error:
    • Ambiguous type variable ‘t0’ arising from the literal ‘100’
      prevents the constraint ‘(Num t0)’ from being solved.
      Probable fix: use a type annotation to specify what ‘t0’ should be.
      These potential instances exist:
        instance Num Integer -- Defined in ‘GHC.Num’
        instance Num Double -- Defined in ‘GHC.Float’
        instance Num Float -- Defined in ‘GHC.Float’
        ...plus two others
        (use -fprint-potential-instances to see them all)
    • In the second argument of ‘($)’, namely ‘100’
      In the expression: (+ 1) <$> ask' $ 100
      In an equation for ‘foo'’: foo' = (+ 1) <$> ask' $ 100
    |
275 |   (+ 1) <$> ask' $ 100
    |                    ^^^
</code></pre>
