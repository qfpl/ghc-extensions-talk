# Sugar {data-background="images/sugar-coloured-wedges-dark.jpg"}

## `OverloadedStrings`

::: {class="left"}
Enable overloaded string literals via the `IsString` type class.
:::

##

<pre class="no-highlight"><code data-trim data-noescape>
<span class="fragment fade-in-then-semi-out">GHCi, version 8.6.4: http://www.haskell.org/ghc/  :? for help
Loaded GHCi configuration from /home/andrew/git/dot-files/.ghci
λ> :t "Lambda"
"Lambda" :: [Char]</span>
<span class="fragment">λ> :set -XOverloadedStrings 
λ> :t "Jam"
"Jam" :: Data.String.IsString p => p</span>
</code></pre>


##

<pre class="haskell"><code data-trim data-noescape>
<span class="fragment fade-in-then-semi-out">class IsString a where
  fromString :: String -> a</span>
<div class="fragment fade-in-then-semi-out">
instance IsString Text where
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
<span class="fragment fade-in-then-semi-out">(* 2)</span>

<span class="fragment fade-in-then-semi-out">(,True)</span>

<span class="fragment fade-in-then-semi-out">\x -> (x,True)</span>
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
<span class="fragment fade-in-then-semi-out" data-fragment-index="1">instance (Traversable f, Traversable g) => Traversable (Compose f g)</span>
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

```haskell
  if 1 < 0 then
    "foo"
  else if 12 > 4 then
    "bar"
  else if even 42 then
    "42"
  else
    "no idea"
```

##

```haskell
  if | 1 < 0 -> "foo"
     | 12 > 4 -> "bar"
     | even 42 -> "42"
     | otherwise -> "no idea"
```
