# WIP

## Not so benign

::: {.notes}
- Seen some people claim that FlexibleInstances is benign.
- Often is, but it can bite you if you're not careful.
:::

##

<pre class="haskell"><code data-trim data-noescape>
module FIA where

data A = A1 | A2 deriving (Eq, Ord, Show)

data Whoopsie a b c =
  Whoopsie a b c
  deriving (Eq, Show)
</code></pre>

##

<pre class="haskell"><code data-trim data-noescape>
<span class="fragment fade-in-then-semi-out" data-fragment-index="1">{-# LANGUAGE FlexibleInstances #-}</span>

<span class="fragment fade-in-then-semi-out" data-fragment-index="2">module FIB where

import Data.Set (Set, insert)
import FIA</span>

<span class="fragment fade-in-then-semi-out" data-fragment-index="3">data B = B deriving (Eq, Ord, Show)</span>

<span class="fragment fade-in-then-semi-out" data-fragment-index="4">instance Ord c => </span><span class="fragment" data-fragment-index="4">Ord (Whoopsie A B c)</span><span class="fragment fade-in-then-semi-out" data-fragment-index="4"> where
  compare (Whoopsie a1 b1 c1) (Whoopsie a2 b2 c2) =</span>
    <span class="fragment" data-fragment-index="4">compare a1 a2</span><span class="fragment fade-in-then-semi-out" data-fragment-index="4"> <> compare b1 b2 <> compare c1 c2</span><span class="fragment" data-fragment-index="5"></span>

<span class="fragment fade-in-then-semi-out" data-fragment-index="6">insB :: Ord c => Whoopsie A B c -> Set (Whoopsie A B c) -> Set (Whoopsie A B c)
insB = insert</span>
</code></pre>

##

<pre class="haskell"><code data-trim data-noescape>
<span class="fragment fade-in-then-semi-out" data-fragment-index="1">{-# LANGUAGE FlexibleInstances #-}

module FIC where

import Data.Set (Set, insert)
import FIA

data C = C deriving (Eq, Ord, Show)</span>

<span class="fragment fade-in-then-semi-out" data-fragment-index="2">instance Ord b => </span><span class="fragment" data-fragment-index="2">Ord (Whoopsie A b C)</span><span class="fragment fade-in-then-semi-out" data-fragment-index="2"> where
  compare (Whoopsie a1 b1 c1) (Whoopsie a2 b2 c2) =</span>
    <span class="fragment" data-fragment-index="2">compare a2 a1</span><span class="fragment fade-in-then-semi-out" data-fragment-index="2"> <> compare b1 b2 <> compare c1 c2</span><span class="fragment" data-fragment-index="3"></span>

<span class="fragment" data-fragment-index="4">insC :: Ord b => Whoopsie A b C -> Set (Whoopsie A b C) -> Set (Whoopsie A b C)
insC = insert</span>
</code></pre>

##

<pre class="haskell"><code data-trim data-noescape>
<span class="fragment fade-in-then-semi-out">module Main where

import Data.Set (Set, empty)

import FIA
import FIB
import FIC

test :: Set (Whoopsie A B C)
test =</span>
  <span class="fragment">insB (Whoopsie A1 B C) . </span><span class="fragment">insC (Whoopsie A1 B C) . </span><span class="fragment">insC (Whoopsie A2 B C) $ empty</span>

<span class="fragment">main :: IO ()
main =
  print test</span>
</code></pre>

##


<pre class="no-highlight"><code data-trim data-noescape>
<span class="fragment fade-in-then-semi-out">$ ghc --version
The Glorious Glasgow Haskell Compilation System, version 8.4.4</span>

<span class="fragment fade-in-then-semi-out">$ ghc -Wall -fforce-recomp Main.hs -o whoopsie</span>
<span class="fragment fade-in-then-semi-out">[1 of 4] Compiling FIA              ( FIA.hs, FIA.o )
[2 of 4] Compiling FIB              ( FIB.hs, FIB.o )
[3 of 4] Compiling FIC              ( FIC.hs, FIC.o )
[4 of 4] Compiling Main             ( Main.hs, Main.o )
Linking whoopsie ...</span>

<span class="fragment fade-in-then-semi-out">> ./whoopsie
fromList [Whoopsie A1 B C,Whoopsie A2 B C,Whoopsie A1 B C]</span>
</code></pre>
