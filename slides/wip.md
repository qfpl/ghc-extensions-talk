# WIP

## `FlexibleContexts`

::: {.left}
Relax some of the requirements regarding contexts.
:::

##

<pre class="no-highlight"><code data-trim data-noescape>
updateThing ::
  MonadState MyState m
  => m ()
</code></pre>

::: {.notes}
- Requires FlexibleContexts because of the non type-variable argument to `MonadState`
- Fairly common to specify concrete type for environment/state.
:::

##

<pre class="no-highlight"><code data-trim data-noescape>
updateThing ::
  ( HasThing s
  , MonadState s m
  )
  => m ()
</code></pre>

::: {.notes}
Counterpoint is that often this isn't necessary and we can add constraints, remain polymorphic,
and maximise reuse.
:::
