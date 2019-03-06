# GHC language extensions

### Abstract

Language extensions are everywhere in the modern Haskell world. As beginners we are often told by instructors or the compiler itself to enable particular extensions to allow for some syntactic sugar or enable a common feature. If we continue to use Haskell, we likely come to depend on even more language extensions, until it is common place for the first 10 lines of each file to be language pragmas. Many of us, myself included, are probably guilty of enabling extensions without fully understanding what they do, or understanding what tradeoffs and risks they might present. I'd like to do better.

We'll start with what language extensions are, and the different ways they may be enabled. From there, we'll look at some of the simpler extensions that give us some syntactic sugar. Next we'll look at some heavier weight extensions, including those commonly used to alter the type class mechanism. There are over 100 language extensions supported by GHC 8.6, so it is not my intention to cover all or even most of them. Instead, our focus will be extensions that are commonly used, their motivation, their use, and any risks/tradeoffs they present.

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

A high level outline for the talk is as follows. Note that the extensions included may change depending on time available, and what further research uncovers.

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
   + InstanceSigs
- Heavier weight
   + NamedFieldPuns
   + RecordWildCards
   + GeneralizedNewtypeDeriving
   + ScopedTypeVariables
- Type class extensions
   + MultiParamTypeClasses
   + FlexibleInstances
   + FlexibleContexts
   + UndecidableInstances
   
For each language extension covered, motivation and usage examples will be covered. Risks and tradeoffs will also be uncovered as appropriate.

### Learning outcomes

Attendees will come away with an awareness of some commonly used language extensions, as well as an understanding of what problems they solve, and what risks and tradeoffs they come with.

### Links

Links for Andrew's previous conference talks (YLJ 2018, Compose 2018, and Compose 2017) are here:

http://qfpl.io/people/ajmcmiddlin/

### Labels

Haskell, language extensions, GHC
