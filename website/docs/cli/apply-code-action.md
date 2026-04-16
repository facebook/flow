---
title: Apply Code Action
slug: /cli/apply-code-action
description: "Apply LSP code actions like autofixes and code transformations from the command line, useful for scripting and batch-fixing."
---

The `apply-code-action` command exposes LSP code actions via the CLI, allowing you to apply autofixes and code transformations without an editor. This is useful for scripting, CI pipelines, or batch-fixing files from the command line.

## Usage {#toc-usage}

```sh
flow apply-code-action SUBCOMMAND [OPTIONS]... FILE
```

The command requires a running Flow server. If one is not already running, it will be started automatically.

## Subcommands {#toc-subcommands}

### `experimental.quickfix` {#toc-experimental-quickfix}

Applies all safe quickfixes to a file. These are the same quickfixes that your editor shows via LSP code actions, such as adding suppression comments or replacing deprecated syntax.

```sh
flow apply-code-action 'experimental.quickfix' [OPTIONS] FILE
```

By default, only safe quickfixes are applied. You can optionally include one additional best-effort fix using the `--include-best-effort-fix` flag. Best-effort fixes may change the runtime semantics of your program (for example, adding a type annotation to a variable declaration to satisfy a type error).

**Options:**

- `--in-place`: Overwrite the input file with the result. Without this flag, the transformed file contents are printed to stdout.
- `--include-best-effort-fix`: Include one additional best-effort quickfix beyond the safe fixes.

**Example:**

```sh
# Print the file with all safe quickfixes applied
flow apply-code-action 'experimental.quickfix' src/app.js

# Apply safe quickfixes and overwrite the file
flow apply-code-action 'experimental.quickfix' --in-place src/app.js

# Also apply one best-effort fix
flow apply-code-action 'experimental.quickfix' --include-best-effort-fix src/app.js
```

### `source.addMissingImports` {#toc-source-add-missing-imports}

Adds missing import statements for unresolved references in a file. Flow resolves each unbound name to an export in your project and inserts the appropriate `import` statement.

```sh
flow apply-code-action 'source.addMissingImports' [OPTIONS] FILE
```

**Options:**

- `--in-place`: Overwrite the input file with the result. Without this flag, the transformed file contents are printed to stdout.

**Example:**

Given a file `app.js` that references `OtherModule` without importing it:

```js
// @flow

OtherModule;
```

Running the command:

```sh
flow apply-code-action 'source.addMissingImports' app.js
```

produces output with the import added:

```js
// @flow

import OtherModule from "./OtherModule";

OtherModule;
```

To update the file in place:

```sh
flow apply-code-action 'source.addMissingImports' --in-place app.js
```

### `suggestImports` {#toc-suggest-imports}

Lists import suggestions for all unbound names in a file, ranked by usage. Unlike the other subcommands, this does not modify the file. Instead, it outputs a JSON object describing the available import code actions for each unresolved name.

```sh
flow apply-code-action suggestImports [OPTIONS] FILE
```

**Options:**

- `--json`: Output results in JSON format (default behavior for this subcommand).
- `--pretty`: Pretty-print the JSON output.

**Example:**

```sh
flow apply-code-action suggestImports --pretty src/app.js
```

The output is a JSON object keyed by unbound name, where each value is an array of possible import actions:

```json
{
  "OtherModule": [
    {
      "title": "Import default from ./OtherModule",
      "kind": "quickfix",
      "edit": {
        "changes": {
          "file:///path/to/app.js": [
            {
              "newText": "import OtherModule from \"./OtherModule\";\n\n",
              "range": {
                "start": { "character": 0, "line": 1 },
                "end": { "character": 0, "line": 1 }
              }
            }
          ]
        }
      }
    }
  ]
}
```

When multiple modules export the same name, all candidates are listed so you can choose the appropriate one.
