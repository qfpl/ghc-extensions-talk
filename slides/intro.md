#

<pre class="no-highlight"><code data-trim data-noescape>
type-class-extensions.lhs:3:3: error:
    • Too many parameters for class ‘Foo’
      <span class="fragment highlight-current-green">(Enable MultiParamTypeClasses to allow multi-parameter classes)</span>
    • In the class declaration for ‘Foo’
  |
3 | > class Foo a b where
  |   ^^^^^^^^^^^^^^^^^^^...
</code></pre>

::: notes
 - Raise hands if you've seen an error like this before?
 - Keep them up if you've enabled the extension without knowing why you needed it.
 - I'm guilty too --- thought it was time to look at them in more detail.
:::

##

```haskell
{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TupleSections         #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}
```

::: notes
 - Taken from `servant` --- `Servant.API.ContentTypes`
:::

