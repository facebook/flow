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
  cycle           Output .dot file for cycle containing the given file
  find-module     Resolves a module reference to a file
  find-refs       Gets the reference locations of a variable or property
  focus-check     EXPERIMENTAL: Does a focused Flow check on a file (and its dependents and their dependencies) and prints the results
  force-recheck   Forces the server to recheck a given list of files
  gen-flow-files  EXPERIMENTAL: Generate minimal .js.flow files for publishing to npm.
  get-def         Gets the definition location of a variable or property
  get-imports     Get names of all modules imported by one or more given modules
  ide             Starts a persistent connection to the server. Currently in development and highly unstable
  init            Initializes a directory to be used as a flow root directory
  ls              Lists files visible to Flow
  port            Shows ported type annotations for given files
  server          Runs a Flow server in the foreground
  start           Starts a Flow server
  status          (default) Shows current Flow errors by asking the Flow server
  stop            Stops a Flow server
  suggest         Shows type annotation suggestions for given files
  type-at-pos     Shows the type at a given file and position
  version         Print version information

Default values if unspecified:
  COMMAND       status

Status command options:
  --color                           Display terminal output in color. never, always, auto (default: auto)
  --from                            Specify client (for use by editor plugins)
  --help                            This list of options
  --ignore-version                  Ignore the version constraint in .flowconfig
  --include-warnings                Include warnings in the error output (warnings are excluded by default)
  --json                            Output results in JSON format
  --json-version                    The version of the JSON format (defaults to 1)
  --max-warnings                    Warnings above this number will cause a nonzero exit code (implies --include-warnings)
  --message-width                   Sets the width of messages but not code snippets (defaults to the smaller of 120 or the terminal width)
  --no-auto-start                   If the server is not running, do not start it; just exit
  --one-line                        Escapes newlines so that each error prints on one line
  --pretty                          Pretty-print JSON output (implies --json)
  --quiet                           Suppress output about server startup
  --retries                         Set the number of retries. (default: 3)
  --retry-if-init                   retry if the server is initializing (default: true)
  --sharedmemory-dep-table-pow      The exponent for the size of the shared memory dependency table. The default is 17, implying a size of 2^17 bytes
  --sharedmemory-dirs               Directory in which to store shared memory heap (default: /dev/shm/)
  --sharedmemory-hash-table-pow     The exponent for the size of the shared memory hash table. The default is 19, implying a size of 2^19 bytes
  --sharedmemory-log-level          The logging level for shared memory statistics. 0=none, 1=some
  --sharedmemory-minimum-available  Flow will only use a filesystem for shared memory if it has at least these many bytes available (default: 536870912 - which is 512MB)
  --show-all-branches               Print all branch errors (the default is to print the most relevant branches)
  --show-all-errors                 Print all errors (the default is to truncate after 50 errors)
  --strip-root                      Print paths without the root
  --temp-dir                        Directory in which to store temp files (default: FLOW_TEMP_DIR, or /tmp/flow/)
  --timeout                         Maximum time to wait, in seconds
  --unicode                         Display terminal output with unicode decoration. never, always, auto (default: auto)
  --version                         (Deprecated, use `flow version` instead) Print version number and exit
```

You can then, further dig into particular COMMANDs by adding the `--help` flag.

So, for example, if you want to know more about how the autocomplete works, you
can use this command:

```sh
flow autocomplete --help
```
