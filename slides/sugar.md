# Sugar {data-background="images/sugar-coloured-wedges-dark.jpg"}

## `OverloadedStrings`

::: {class="left"}
Enable overloaded string literals via the `IsString` type class.
:::

##

<pre class="haskell"><code data-trim data-noescape>
<div class="fragment fade-in-then-semi-out">-- Your code on Haskell 2010
"foo" :: String
</div>
<div class="fragment fade-in-then-semi-out">-- Your code on OverloadedStrings
"foo" :: IsString a => a
</div>
<div class="fragment fade-in-then-semi-out">class IsString a where
  fromString :: String -> a
</div>
<div class="fragment fade-in-then-semi-out">instance IsString [Char] where
  fromString cs = cs
</div>
</code></pre>

##

<pre class="haskell"><code data-trim data-noescape>
<div class="fragment fade-in-then-semi-out">instance IsString Text where
  fromString = pack
</div>
<span class="fragment fade-in-then-semi-out">isGood :: Text -> Bool</span>

<span class="fragment fade-in-then-semi-out">isGood "foo"</span>
</code></pre>

## `TupleSections`

::: {class="left"}
Allow partially applied tuple constructors.
:::

##

<pre class="haskell"><code data-trim data-noescape>
<span class="fragment fade-in-then-semi-out">-- Replace this
\x -> (x,True)
</span>
<span class="fragment fade-in-then-semi-out">-- With this
(,True)
</span>
</code></pre>

##

<pre class="haskell"><code data-trim data-noescape>
(,True,,,42::Int,)<span class="fragment fade-in"> :: a -> b -> c -> d -> (a,Bool,b,c,Int,d)</span>
</code></pre>

## `InstanceSigs`

::: {class="left"}
Allow type signatures for definitions of instance members.
:::

##

<pre class="haskell"><code data-trim data-noescape>
<span class="fragment fade-in-then-semi-out" data-fragment-index="1">instance Traversable (Compose f g)</span>
  <span class="fragment fade-in" data-fragment-index="2">traverse :: (a -> h b) -> Compose f g a -> h (Compose f g b)</span>
  <span class="fragment fade-in-then-semi-out" data-fragment-index="1">traverse = undefined</span>
</code></pre>

## `LambdaCase`

::: {class="left"}
Adds syntactic sugar for pattern matching on a function's argument.
:::

##

<pre class="haskell"><code data-trim data-noescape>
pretty ::
  -> Expr
  -> Text
pretty <span class="fragment highlight-red">e = case e of</span>
  LitI n -> pack $ show n
  LitB True -> "true"
  LitB False -> "false"
</code></pre>

##

```haskell
pretty ::
  -> Expr
  -> Text
pretty = \case
  LitI n -> pack $ show n
  LitB True -> "true"
  LitB False -> "false"
```

## `MultiWayIf`

::: {class="left"}
Adds syntactic sugar for nested `if-then-else` expressions.
:::

##


