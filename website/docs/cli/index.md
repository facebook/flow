---
title: Flow CLI
slug: /cli
description: How to use Flow from the command line. Including how to manage the Flow background process.
---

The flow command line tool is made to be easy-to-use for simple cases.

Using the command `flow` will type-check your current directory if the
`.flowconfig` file is present. A flow server will automatically be started if
needed.

The CLI tool also provides several other options and commands that allow you to
control the server and build tools that integrate with Flow.

To find out more about the CLI just type:

```sh
flow --help
```

This will give you information about everything that flow can do. Running this
command should print something like this:

```
Usage: flow [COMMAND]

Valid values for COMMAND:
  ast             Print the AST
  autocomplete    Queries autocompletion information
  batch-coverage  Shows aggregate coverage information for a group of files or directories
  check           (default) Shows current Flow errors by asking the Flow server
  check-contents  Run typechecker on contents from stdin
  codemod         Runs large-scale codebase refactors
  config          Read or write the .flowconfig file
  coverage        Shows coverage information for a given file
  cycle           Output .dot file for cycle containing the given file
  find-module     Resolves a module reference to a file
  focus-check     Type-checks specific files and their dependents in the foreground (no server)
  force-recheck   Forces the server to recheck a given list of files
  full-check      Type-checks all files in the foreground (no server, can be slow on large
                  codebases)
  get-def         Gets the definition location of a variable or property
  graph           Outputs dependency graphs of flow repositories
  init            Initializes a directory to be used as a flow root directory
  ls              Lists files visible to Flow
  lsp             Acts as a server for the Language Server Protocol over stdin/stdout [experimental]
  server          Runs a Flow server in the foreground
  start           Starts a Flow server
  status          (default) Shows current Flow errors by asking the Flow server
  stop            Stops a Flow server
  type-at-pos     Shows the type at a given file and position
  version         Print version information

Default values if unspecified:
  COMMAND         status

Status command options:
  --color              Display terminal output in color. never, always, auto (default: auto)
  --from               Specify who is calling this CLI command (used by logging)
  --help               This list of options
  --include-warnings   Include warnings in the error output (warnings are excluded by default)
  --json               Output results in JSON format
  --json-version       The version of the JSON format (defaults to 1)
  --max-warnings       Warnings above this number will cause a nonzero exit code (implies
                       --include-warnings)
  --no-auto-start      If the server is not running, do not start it; just exit
  --one-line           Escapes newlines so that each error prints on one line
  --pretty             Pretty-print JSON output (implies --json)
  --quiet              Suppress output about server startup
  --retries            Set the number of retries. (default: 3)
  --show-all-branches  Print all branch errors (the default is to print the most relevant branches)
  --show-all-errors    Print all errors (the default is to truncate after 50 errors)
  --strip-root         Print paths without the root
  --temp-dir           Directory in which to store temp files (default: FLOW_TEMP_DIR, or
                       /tmp/flow/)
  --timeout            Maximum time to wait, in seconds
  --version            Print version number and exit
```

Example with custom project root:
```sh
mydir
├── frontend
│   ├── .flowconfig
│   └── app.js
└── backend
```

```sh
flow frontend
```

You can then, further dig into particular COMMANDs by adding the `--help` flag.

So, for example, if you want to know more about how the autocomplete works, you
can use this command:

```sh
flow autocomplete --help
```
