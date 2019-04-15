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

```haskell
instance Traversable (Compose f g)
  traverse :: (a -> h b) -> Compose f g a -> h (Compose f g b)
  traverse = _ -- help!
```

## `LambdaCase`

## `MultiWayIf`
