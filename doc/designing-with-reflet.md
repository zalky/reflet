# Design Notes

1. Careful with interactions between `:component-did-mount` and
`:component-will-unmount` (which includes reaction and with-let
on-dispose methods) across lifecycles:

There is no way for mount methods to safely see any state effects of
unmount methods from a previous lifecycle, assuming proper usage where
we do not deref reactions/subscriptions anywhere outside the render
phase.

This has important concurrency implications: you can easily create
race conditions if the `:component-will-unmount` and the
`:component-did-mount` produce mutations on the same app state.

To understand why, consider the order of React lifecycle mount and
unmounts:

   1. Components are constructed with initial props. Closures are
      created around the lifecycle methods.

   2. React runs an initial render with initial props.

   3. React computes which components are no longer needed from
      previous lifecycles, and their `:component-will-unmount` methods
      `with-let` finally clauses, and reaction on-dispose functions
      are called.

   4. The `:ref` callback functions fire for newly mounted
      components. However, these functions have closed over the
      initial props, and will not see any changes in props from the
      unmount methods. (And these callbacks do not technically happen
      in the render so it is not safe to dereference ratoms/reactions
      in callbacks to `:ref`)

   5. The `:component-did-mount` is called, but with the inital
      props. Any effects from the `:component-will-unmount` or
      on-dispose methods will not be seen in these props.

   6. Now components may finally have a chance to re-render in
      response to changes from unmount methods. But by now the
      `:component-did-mount` lifecycle is long gone.

3. Careful when passing transient refs around outside the component
   tree in which they were made. In a very real way they are tied to
   things that are essentially ephemeral. It is very easy to end up
   writing or reading state after the state has been cleaned
   up. Reflet will warn you about this, but it is best avoided. The
   two Golden Rules are:

   1. Do not use transient refs as joins (map values or link values)
      in db writes
   2. Do not pass them to contexts outside of the `with-ref` in which
      they were made

4. Careful with where you supply refs and where you supply uuids.

5. Careful not to confuse props attributes with unique attributes,
   especially when merging.
