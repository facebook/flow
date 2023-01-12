---
title: "New Implementation of Unions and Intersections"
short-title: "New Unions and Intersections"
author: "Sam Goldman"
hide_table_of_contents: true
---

### Summary

Before Flow 0.28, the implementation of union/intersection types had serious
bugs and was [the][gh1759] [root][gh1664] [cause][gh1663] [of][gh1462]
[a][gh1455] [lot][gh1371] [of][gh1349] [weird][gh842] [behaviors][gh815] you
may have run into with Flow in the past. These bugs have now been addressed in
[a diff landing in 0.28][fotu].

<!--truncate-->

As you might expect after a major rewrite of a tricky part of the type system
implementation, there will be a short period of adjustment: you may run into
kinks that we will try to iron out promptly, and you may run into some
unfamiliar error messages.

### New Error Messages

```
<error location> Could not decide which case to select
<location of union/intersection type>

  Case 1 may work:
  <location of 1st case of union/intersection type>

  But if it doesn't, case 2 looks promising too:
  <location of 2nd case of union/intersection type>

  Please provide additional annotation(s) to determine whether case 1 works
  (or consider merging it with case 2):
  <location to annotate>
  <location to annotate>
  ...
```

What this means is that at `<error location>`, Flow needs to make a choice: one
of the members of the union/intersection type at
`<location of union/intersection type>` must be applied, but Flow can't choose
safely based on available information. In particular, it cannot decide between
case `1` and `2`, so Flow lists a bunch of annotations that can help it
disambiguate the two cases.

### Actions Needed

You can fix the errors in two ways:

- Actually go and annotate the listed locations. This should be by far the most
  common fix.
- Discover that there is real, unintentional ambiguity between case 1 and 2,
  and rewrite the two cases in the union type to remove the ambiguity. When
  this happens, typically it will fix a large number of errors.

There are two more possibilities, however:

- There's no real ambiguity and Flow is being too conservative / dumb. In this
  case, go ahead and do the annotations anyway and file an issue on GitHub. We
  plan to do a lot of short-term follow-up work to disambiguate more cases
  automatically, so over time you should see less of (3).
- You have no idea what's going on. The cases being pointed to don't make sense.
  They don't correspond to what you have at `<error location>`. Hopefully you
  won't run into (4) too often, but if you do **please file an issue**, since
  this means there are still latent bugs in the implementation.

If you file an issue on GitHub, please include code to reproduce the issue. You
can use [Try Flow](https://flowtype.org/try/) to share your repro case easily.

If you're curious about the whys and hows of these new error messages, here's
an excerpt from the commit message of the "fate of the union" diff:

### Problem

Flow's inference engine is designed to find more errors over time as
constraints are added...but it is not designed to backtrack. Unfortunately,
checking the type of an expression against a union type does need backtracking:
if some branch of the union doesn't work out, the next branch must be tried,
and so on. (The same is true for checks that involve intersection types.)

The situation is further complicated by the fact that the type of the
expression may not be completely known at the point of checking, so that a
branch that looks promising now might turn out to be incorrect later.

### Solution

The basic idea is to delay trying a branch until a point where we can decide
whether the branch will definitely fail or succeed, without further
information. If trying a branch results in failure, we can move on to the next
branch without needing to backtrack. If a branch succeeds, we are done. The
final case is where the branch looks promising, but we cannot be sure without
adding constraints: in this case we try other branches, and *bail* when we run
into ambiguities...requesting additional annotations to decide which branch to
select. Overall, this means that (1) we never commit to a branch that might
turn out to be incorrect and (2) can always select a correct branch (if such
exists) given enough annotations.

[gh1759]: https://github.com/facebook/flow/issues/1759
[gh1664]: https://github.com/facebook/flow/issues/1664
[gh1663]: https://github.com/facebook/flow/issues/1663
[gh1462]: https://github.com/facebook/flow/issues/1462
[gh1455]: https://github.com/facebook/flow/issues/1455
[gh1371]: https://github.com/facebook/flow/issues/1371
[gh1349]: https://github.com/facebook/flow/issues/1349
[gh842]: https://github.com/facebook/flow/issues/824
[gh815]: https://github.com/facebook/flow/issues/815
[fotu]: https://github.com/facebook/flow/commit/2df7671e7bda770b95e6b1eaede96d7a8ab1f2ac
