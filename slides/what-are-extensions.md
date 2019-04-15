# Language extensions 101

## Haskell 2010

::: {class="left fragment fade-in-then-semi-out"}
There are multiple versions of the Haskell programming language.
:::

::: {class="left fragment fade-in-then-semi-out"}
Most people are probably using Haskell 2010.
:::

::: {class="left fragment fade-in-then-semi-out"}
Haskell 2010 is defined in the [Haskell 2010 Language Report](https://www.haskell.org/onlinereport/haskell2010/haskell.html).
:::

## What's not in Haskell 2010?

::: {class="left fragment fade-in-then-semi-out"}
Haskell 2010 _doesn't_ include a bunch of language features that you might think of as being part of Haskell.
:::

::: {class="left"}
- Type classes with more than one parameter.
- String literals for anything other than `[Char]`
- Generalised Algebraic Data Types (GADTs)
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

- `{-# LANGUAGE #-}` pragmas
- `default-extensions` field in a `.cabal` file
- GHC flag
- `:set -X` in GHC

##

```haskell
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs, ScopedTypeVariables #-}
```

##

```
default-extensions:    OverloadedStrings
                     , GADTs
                     , ScopedTypeVariables
```

##

```
ghc -XOverloadedStrings Foo.hs
```

##

```
$ ghci
λ :set -XOverloadedStrings
```
