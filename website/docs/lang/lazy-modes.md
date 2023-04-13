---
title: Lazy Mode
slug: /lang/lazy-modes
---

By default, the Flow server will typecheck all your code. This way it can answer
questions like "are there any Flow errors anywhere in my code". This is very
useful for tooling, like a continuous integration hook which prevents code
changes which introduce Flow errors.

However, sometimes a Flow user might not care about all the code. If they are
editing a file `foo.js`, they might only want Flow to typecheck the subset of
the repository needed to answer questions about `foo.js`. Since Flow would only
check a smaller number of files, this would be faster. This is the motivation
behind Flow's lazy mode.

## Classifying Files {#toc-classifying-files}

Lazy mode classifes your code into four categories:

1. **Focused files**. These are the files which the user cares about.
2. **Dependent files**. These are the files which depend on the focused files.
Changes to the focused files might cause type errors in the dependent files.
3. **Dependency files**. These are the files which are needed in order to
typecheck the focused or dependent files.
4. **Unchecked files**. All other files.

Lazy mode will still find all the JavaScript files and parse them. But it won't
typecheck the unchecked files.

## Choosing Focused Files {#toc-choosing-focused-files}

Flow will focus files when they change on disk, using Flow's built-in file watcher
("dfind") or Watchman.

So, all files that change while Flow is running will be focused. But what about
files that change when Flow is not running? If you're using Git or Mercurial,
Flow will ask it for all of the files that have changed since the mergebase
with "master" (the common ancestor of the current commit and the master branch).

If you're not using "master" (e.g. "main" instead), you can change this with
the `file_watcher.mergebase_with` config. If you're working from a clone, you
might want to set this to "origin/master" (for Git), which will focus all files
that have changed locally, even if you commit to your local "master" branch.

The net result is that Flow will find the same errors in lazy mode as in a full
check, so long as there are no errors upstream. For example, if your CI ensures
that there are no errors in "master," then it's redundant for Flow to check all
of the unchanged files for errors that can't exist.

## Using Lazy Mode {#toc-using-lazy-mode}

To enable lazy mode, set `lazy_mode=true` in the `.flowconfig`.

To start Flow in lazy mode manually, run

```bash
flow start --lazy-mode true
```

## Forcing Flow to Treat a File as Focused {#toc-forcing-flow-to-treat-a-file-as-focused}

You can force Flow to treat one or more files as focused from the CLI.

```bash
flow force-recheck --focus path/to/A.js path/to/B.js
```
