# GHC language extensions

### Abstract

Language extensions are everywhere in the modern Haskell world. As beginners we are often told by instructors or the compiler itself to enable particular extensions to allow for some syntactic sugar or enable a feature. However, the compiler isn't always right, and language extensions may come with risks or penalties we are not aware of.

Furthermore, there are over 100 language extensions.

In this talk, Andrew aims to explore the world of

### Category

Language

### Level

Intermediate

### Session type

talk

### Target audience

Haskell users who want to get exposed to language extensions they may not be familiar with, or who want a deeper understanding of the language extensions they already know and use.

### Session prerequisite

A basic knowledge of Haskell is expected. The talk progresses from things a novice would find useful through to content aimed at intermediate/advanced users.

### Outline

The talk progresses from beginner-friendly material through to topics aimed at an intermediate or advanced Haskell user. The intention is that the beginner material is covered quickly for completeness, and the majority of the talk is spent looking at the details and tradeoffs related to a handful of common extensions.

A high level outline for the talk is as follows.

- What are language extensions?
- How are language extensions enabled?
   + Compiler pragmas (as specified in Haskell 2010)
   + Cabal directives
   + Compiler flag
   + GHCi
- Syntactic sugar
   + LambdaCase
   + TupleSections
   + MultiWayIf
   + OverloadedStrings
- Heavier weight
   + RecordWildCards
   + GeneralizedNewtypeDeriving
   + ScopedTypeVariables
- Type class extensions
   + MultiParamTypeClasses
   + FlexibleInstances
   + FlexibleContexts
   + UndecidableInstances
   
For each language extension covered, motivation and usage examples will be examined. In some cases, risks and tradeoffs will also be uncovered.

### Learning outcomes

Attendees will come away with an awareness of some commonly used language extensions, as well as an understanding of what problems they solve, and what risks and tradeoffs they come with.

### Links

Links for Andrew's previous conference talks (YLJ 2018, Compose 2018, and Compose 2017) are here:

http://qfpl.io/people/ajmcmiddlin/

### Labels

Haskell, language extensions, GHC
