# Language extensions 101 {data-background="images/extensions.png"}

## Haskell 2010

::: {.left}
Haskell 2010 is defined in the [Haskell 2010 Language Report](https://www.haskell.org/onlinereport/haskell2010/haskell.html).
:::

::: {.notes}
There are multiple versions of the Haskell programming language.

Most people are probably using Haskell 2010.
:::

## What's not in Haskell 2010?

::: {class="left"}
- Type classes with more than one parameter.
- String literals for anything other than `[Char]`
- Generalised Algebraic Data Types (GADTs)
:::

::: {.notes}
Might think of these as standard.
:::

## Language extensions

::: {class="left fragment fade-in-then-semi-out"}
[Chapter 12](https://www.haskell.org/onlinereport/haskell2010/haskellch12.html#x19-18800012) of the Haskell 2010 report covers compiler pragmas.
:::

::: {class="left fragment fade-in-then-semi-out"}
[Section 12.3](https://www.haskell.org/onlinereport/haskell2010/haskellch12.html#x19-19100012.3) covers the `LANGUAGE` pragma, which is used for extensions.
:::

::: {class="left fragment fade-in-then-semi-out"}
Support for language extensions is encouraged, but not required.
:::

## Enabling extensions in GHC

<!-- We're about to go through each of these in more detail --- cut to the chase -->
<!-- - `{-# LANGUAGE #-}` pragmas -->
<!-- - `default-extensions` field in a `.cabal` file -->
<!-- - GHC flag -->
<!-- - `:set -X` in GHC -->

##

```haskell
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs, ScopedTypeVariables #-}
```

##

```no-highlight
default-extensions:    OverloadedStrings
                     , GADTs
                     , ScopedTypeVariables
```

##

```no-highlight
ghc -XOverloadedStrings Foo.hs
```

##

```no-highlight
$ ghci
λ :set -XOverloadedStrings
```
