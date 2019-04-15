# Sugar {data-background="images/sugar-coloured-wedges-dark.jpg"}

## `OverloadedStrings`

::: {class="left fragment fade-in-then-semi-out"}
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

## `InstanceSigs`

## `LambdaCase`

## `MultiWayIf`
