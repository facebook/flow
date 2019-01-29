---
layout: guide
---

By default, the Flow server will typecheck all your code. This way it can answer
questions like "are there any Flow errors anywhere in my code". This is very
useful for tooling, like a continuous integration hook which prevents code
changes which introduce Flow errors.

However, sometimes a Flow user might not care about all the code. If they are
editing a file `foo.js`, they might only want Flow to typecheck the subset of
the repository needed to answer questions about `foo.js`. Since Flow would only
check a smaller number of files, this would be faster. This is the motivation
behind Flow's lazy modes.

### Classifying Files <a class="toc" id="toc-classifying-files" href="#toc-classifying-files"></a>

Lazy mode tries to classify your code into four categories.

1. **Focused files**. These are the files which the user cares about.
2. **Dependent files**. These are the files which depend on the focused files.
Changes to the focused files might cause type errors in the dependent files.
3. **Dependency files**. These are the files which are needed in order to
typecheck the focused or dependent files.
4. **Unchecked files**. All other files.

Lazy mode will still find all the JavaScript files and parse them. But it won't
typecheck the unchecked files.

### Choosing Focused Files <a class="toc" id="toc-choosing-focused-files" href="#toc-choosing-focused-files"></a>

There are two ways which Flow can use to tell which files the user cares about.

1. **IDE lazy mode**. The IDE tells Flow which files have been opened and closed
via `flow ide` (and in the future `flow lsp`). Flow treats any file which has
ever been opened since the Flow server started as focused.
2. **Filesystem lazy mode**. Flow treats any file which has changed on the
filesystem as focused. This mode is easier to use from the commandline, but
a rebase can make every file appear focused.
3. **Watchman lazy mode**. When starting up, Flow treats any file as focused if
it has changed since the mergebase with master (the common ancestor of the
current commit and the master branch). Any subsequent file that changes is also
focused. After a rebase which changes the mergebase, Flow may restart the server
if it estimates that a restart is faster than a recheck.

### Using IDE Lazy Mode <a class="toc" id="toc-using-ide-lazy-mode" href="#toc-using-ide-lazy-mode"></a>

To start a Flow server in IDE lazy mode, you run

```bash
flow server --lazy-mode ide
```

Alternatively, [you can set the lazy mode from the `.flowconfig`](/en/docs/config/options/#toc-lazy-mode-fs-ide-watchman-none).

The IDE needs to integrate with `flow ide` (or in the future `flow lsp`) to tell
Flow which files are open.

### Using Filesystem Lazy Mode <a class="toc" id="toc-using-filesystem-lazy-mode" href="#toc-using-filesystem-lazy-mode"></a>

To start a Flow server in Filesystem lazy mode, you run

```bash
flow server --lazy-mode fs
```

Alternatively, [you can set the lazy mode from the `.flowconfig`](/en/docs/config/options/#toc-lazy-mode-fs-ide-watchman-none).

### Using Watchman Lazy Mode <a class="toc" id="toc-using-watchman-lazy-mode" href="#toc-using-watchman-lazy-mode"></a>

Watchman lazy mode has a few additional requirements.

1. The Flow root must be within a [Mercurial](https://www.mercurial-scm.org/) repository.
2. [Watchman](https://facebook.github.io/watchman/) must be installed and the
   `watchman` binary must be in the PATH.

To start a Flow server in Watchman lazy mode, you run

```bash
flow server --lazy-mode Watchman
```

Alternatively, [you can set the lazy mode from the `.flowconfig`](/en/docs/config/options/#toc-lazy-mode-fs-ide-watchman-none).

### Forcing Flow to Treat a File as Focused <a class="toc" id="toc-forcing-flow-to-treat-a-file-as-focused" href="#toc-forcing-flow-to-treat-a-file-as-focused"></a>

You can force Flow to treat one or more files as focused from the CLI.

```bash
flow force-recheck --focus ./path/to/A.js /path/to/B.js
```
