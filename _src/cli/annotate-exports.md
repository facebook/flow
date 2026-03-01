---
title: Flow Annotate-Exports
slug: /cli/annotate-exports
---

Upgrading to [Types-First](../../lang/types-first) mode may require a substantial
number of type annotations at module boundaries. To help with the process of
upgrading large codebases, we are providing a codemod command, whose goal is to
fill in these missing annotations. This command is included in the Flow binary
in versions `>= 0.125`.

> Note: As of version 0.134, types-first is the default mode. If you are using a
version `>=0.134`, make sure you set `types_first=false` in your .flowconfig while
running this codemod.

This command uses types that Flow infers, to fill in positions that would otherwise
raise *signature-verification* failures. It will include the necessary type import
statements, as long as the respective types are exported from their defining modules.

It is designed for use on multiple files at once, rather than one file at a time.
For this reason it doesn't connect to an existing Flow server, but rather starts
a checking process of its own.

As is typical with such mechanized approaches, it comes with a few caveats:

1. It won’t be able to fill in every required type annotation. Some cases will
require manual effort.
2. Inserted annotations may cause new flow errors, since it’s not always possible
to match inferred type with types that can be written as annotations.
3. File formatting may be affected. If a code formatter (e.g. prettier) is used,
it is recommended that you run it after the codemod has finished running.


### How to apply the codemod {#toc-how-to-apply-the-codemod}

A typical way to invoke this command is

```
flow codemod annotate-exports \
  --write \
  --repeat \
  --log-level info \
  /path/to/folder \
  2> out.log
```

This command will transform files under `/path/to/folder`. This does not need to
be the root directory (the one containing `.flowconfig`).

It uses the following flags:
* `--write` will update files that require annotations under `/path/to/folder`
in-place. Without this flag the resulting files will be printed on the command line.

* `--repeat` ensures that the transformation will be applied until no more files
change. This mode is necessary here, because each new type the codemod adds may
require new locations to be annotated.

* `--log-level info` outputs useful debugging information in the standard error stream.
This option might lead to verbose output, so we're redirecting the error output
to a log file `out.log`.

Another convenient way to provide the input is by passing the flag
```
--input-file file.txt
```
where `file.txt` contains a specific list of files to be transformed.

### Codemod output {#toc-codemod-output}

After each iteration of the codemod, a summary will be printed on the CLI. This
summary includes statistical information about the number of annotations that were
added, and how many locations were skipped. It also prints counts for various kinds
of errors that were encountered. These can be matched to the errors printed in the
logs.

A common error case is when a type `A`, defined in a file `a.js`, but not exported,
is inferred in file `b.js`. The codemod will skip adding this annotation and report
an error in the logs. The fix this case, you can export `A` in `a.js`. Note that
it is not necessary to manually import `A` in `b.js`. The codemod will do this
automatically.
