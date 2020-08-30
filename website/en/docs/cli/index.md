---
layout: guide
---

The flow command line tool is made to be easy-to-use for simple cases.

Using the command `flow` will type-check your current directory if the
`.flowconfig` file is present. A flow server will automatically be started if
needed.

The CLI tool also provides several other options and commands that allow you to
control the server and build tools that integrate with Flow. For example, this
is how the [Nuclide](https://nuclide.io/) editor integrates with Flow to
provide autocompletion, type errors, etc. in its UI.

To find out more about the CLI just type:

```sh
flow --help
```

This will give you information about everything that flow can do. Running this
command should print something like this:

```
Usage: flow [COMMAND] [PROJECT_ROOT]

Valid values for COMMAND:
  ast              Print the AST
  autocomplete     Queries autocompletion information
  batch-coverage   Shows aggregate coverage information for a group of files or directories
  check            Does a full Flow check and prints the results
  check-contents   Run typechecker on contents from stdin
  config           Read or write the .flowconfig file
  coverage         Shows coverage information for a given file
  cycle            Output .dot file for cycle containing the given file
  find-module      Resolves a module reference to a file
  find-refs        Gets the reference locations of a variable or property
  force-recheck    Forces the server to recheck a given list of files
  get-def          Gets the definition location of a variable or property
  get-imports      Get names of all modules imported by one or more given modules
  graph            Outputs dependency graphs of flow repositories
  init             Initializes a directory to be used as a flow root directory
  ls               Lists files visible to Flow
  lsp              Acts as a server for the Language Server Protocol over stdin/stdout [experimental]
  print-signature  Prints the type signature of a file as extracted in types-first mode
  server           Runs a Flow server in the foreground
  start            Starts a Flow server
  status           (default) Shows current Flow errors by asking the Flow server
  stop             Stops a Flow server
  suggest          Provides type annotation suggestions for a given program
  type-at-pos      Shows the type at a given file and position
  version          Print version information

Default values if unspecified:
  COMMAND         status
  PROJECT_ROOT    current folder

Status command options:
  --color              Display terminal output in color. never, always, auto (default: auto)
  --from               Specify client (for use by editor plugins)
  --help               This list of options
  --json               Output results in JSON format
  --no-auto-start      If the server is not running, do not start it; just exit
  --old-output-format  Use old output format (absolute file names, line and column numbers)
  --one-line           Escapes newlines so that each error prints on one line
  --quiet              Suppresses the server-status information that would have been printed to stderr.
  --retries            Set the number of retries. (default: 3)
  --retry-if-init      retry if the server is initializing (default: true)
  --show-all-errors    Print all errors (the default is to truncate after 50 errors)
  --strip-root         Print paths without the root
  --temp-dir           Directory in which to store temp files (default: /tmp/flow/)
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
flow check frontend
```

You can then, further dig into particular COMMANDs by adding the `--help` flag.

So, for example, if you want to know more about how the autocomplete works, you
can use this command:

```sh
flow autocomplete --help
```
