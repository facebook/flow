---
title: .flowconfig [version]
slug: /config/version
---

You can specify in the `.flowconfig` which version of Flow you expect to use.
You do this with the `[version]` section. If this section is omitted or left
blank, then any version is allowed. If a version is specified and not matched,
then Flow will immediately error and exit.

So if you have the following in your `.flowconfig`:

```
[version]
0.22.0
```

and you try to use Flow v0.21.0, then Flow will immediately error with the
message

`"Wrong version of Flow. The config specifies version 0.22.0 but this is version
0.21.0"`

So far, we support the following ways to specify supported versions

- Explicit versions, (e.g. `0.22.0`, which only matches `0.22.0`).
- Intersection ranges, which are ANDed together, (e.g. `>=0.13.0 <0.14.0`,
  which matches `0.13.0` and `0.13.5` but not `0.14.0`).
- Caret ranges, which allow changes that do not modify the left-most non-zero
  digit (e.g.  `^0.13.0` expands into `>=0.13.0 <0.14.0`, and `^0.13.1` expands
  into `>=0.13.1 <0.14.0`, whereas `^1.2.3` expands into `>=1.2.3 <2.0.0`).
