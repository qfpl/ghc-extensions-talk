# WIP

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

