# Type Classes

## Type classes in Haskell 2010

::: {.left}

::: {.fragment}
[Section 4.3.1 of the standard](https://www.haskell.org/onlinereport/haskell2010/haskellch4.html#x10-750004.3) covers type classes.
:::

:::{.fragment}
To summarise, it says that a type class declaration must have the following form.
:::
:::

<pre class="nohighlight fragment"><code data-trim data-noescape>
<span class="fragment highlight-current-green">class</span> <span class="fragment highlight-current-green">cx =></span> <span class="fragment highlight-current-green">C</span> <span class="fragment highlight-current-green">u</span> <span class="fragment highlight-current-green">where cdecls</span>
</code></pre>

::: {.notes}
 - _may_ have a context (`cx => `);
 - _must_ have a class name (`C`);
 - _must_ be parameterised over exactly one type (`u`); and
 - _may_ declare one or more members (`where cdecls`).
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

::: {.fragment}
[Section 4.3.2 of the standard](https://www.haskell.org/onlinereport/haskell2010/haskellch4.html#x10-750004.3)
covers type class instance declarations.
:::

::: {.fragment}
In short, it says that a type class instance must have the following form.
:::

:::

<pre class="fragment no-highlight haskell"><code data-trim data-noescape>
<span class="fragment highlight-current-green">instance</span> <span class="fragment highlight-current-green">cx =></span> <span class="fragment highlight-current-green">C</span> <span class="fragment highlight-current-green">(T u1 … uk)</span> <span class="fragment highlight-current-green">where { d }</span>
</code></pre>

::: {.notes}
  - _may_ have a context (`cx =>`);
  - _must_ mention the class name (`C`);
  - _must_ mention the type the instance is for (`T u1 … uk`); and
  - _may_ contain definitions for the class's members (`{ d }`).
:::
