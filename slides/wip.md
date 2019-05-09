# WIP

## `FlexibleContexts`

::: {.left}
**EXPLANATION HERE.**
:::

##

<pre class="no-highlight"><code data-trim data-noescape>
bar ::
  MonadReader Integer m
  => m Integer
bar =
  (+1) <$> ask
</code></pre>

::: {.notes}

:::
