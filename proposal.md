# GHC language extensions

### Abstract

Language extensions are everywhere in the modern Haskell world. As beginners we are often told by instructors or the compiler itself to enable particular extensions to allow for some syntactic sugar or enable a common feature. If we continue to use Haskell, we likely come to depend on even more language extensions, until it is common place for the first 10 lines of each file to be language pragmas. Many of us, myself included, are probably guilty of enabling extensions without fully understanding what they do, or understanding what tradeoffs and risks they might present. This talk hopes to improve the situation by shedding some light on commonly used extensions.

We'll start with what language extensions are and why they exist. Next we'll consider the different ways they may be enabled. From there, we'll look at some of the simpler extensions that provide syntactic sugar and not much else --- for example, LambdaCase and TupleSections. At this point we'll start to ramp up and look at some heavier weight extensions (e.g. ScopedTypeVariables and GeneralizedNewtypeDeriving), their use cases, and their tradeoffs. Finally, we'll take a more detailed look at some language extensions related to type classes. Specifically; FlexibleInstances, FlexibleContexts, MultiParamTypeClasses, and FunctionalDependencies will be covered.

There are over 100 language extensions supported by GHC 8.6, so it is not my intention to cover all or even most of them. Instead, I hope to explain and demystify some common language extensions and point out that they are not without risks.

### Category

Language

### Level

Intermediate

### Session type

talk

### Target audience

People familiar with Haskell, but who haven't yet used it on larger projects will benefit from at least the first part of the talk.

Intermediate or advanced Haskell users who want to get exposed to language extensions they may not be familiar with, or who want a deeper understanding of the language extensions they already know and use, will get the most out of the latter parts.

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
   + FunctionalDependencies
   + UndecidableInstances
   
For each language extension, its use case and usage examples will be covered. Risks and tradeoffs will also be covered as appropriate.

### Learning outcomes

Attendees will come away with an awareness of some commonly used language extensions, as well as an understanding of what problems they solve, and what risks and tradeoffs they come with. Most importantly, the use cases and risks of some common extensions related to type classes will examined.

### Links

Links for Andrew's previous conference talks (YLJ 2018, Compose 2018, and Compose 2017) are here:

http://qfpl.io/people/ajmcmiddlin/

### Labels

Haskell, language extensions, GHC
