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
Usage: flow [COMMAND]

Valid values for COMMAND:
  ast             Print the AST
  autocomplete    Queries autocompletion information
  check           Does a full Flow check and prints the results
  check-contents  Run typechecker on contents from stdin
  coverage        Shows coverage information for a given file
  find-module     Resolves a module reference to a file
  get-def         Gets the definition location of a variable or property
  get-importers   Gets a list of all importers for one or more given modules
  get-imports     Get names of all modules imported by one or more given modules
  init            Initializes a directory to be used as a flow root directory
  port            Shows ported type annotations for given files
  server          Runs a Flow server in the foreground
  start           Starts a Flow server
  status          (default) Shows current Flow errors by asking the Flow server
  stop            Stops a Flow server
  suggest         Shows type annotation suggestions for given files
  type-at-pos     Shows the type at a given file and position
  version         Print version information

Default values if unspecified:
  COMMAND	status

Status command options:
  --color              Display terminal output in color. never, always, auto (default: auto)
  --from               Specify client (for use by editor plugins)
  --help               This list of options
  --json               Output results in JSON format
  --no-auto-start      If the server is not running, do not start it; just exit
  --old-output-format  Use old output format (absolute file names, line and column numbers)
  --one-line           Escapes newlines so that each error prints on one line
  --retries            Set the number of retries. (default: 3)
  --retry-if-init      retry if the server is initializing (default: true)
  --show-all-errors    Print all errors (the default is to truncate after 50 errors)
  --strip-root         Print paths without the root
  --temp-dir           Directory in which to store temp files (default: /tmp/flow/)
  --timeout            Maximum time to wait, in seconds
  --version            (Deprecated, use `flow version` instead) Print version number and exit
```

You can then, further dig into particular COMMANDs by adding the `--help` flag.

So, for example, if you want to know more about how the autocomplete works, you
can use this command:

```sh
flow autocomplete --help
```
