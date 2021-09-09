---
title: Linting Overview
slug: /linting
---

Flow contains a linting framework that can tell you about more than just type errors. This framework is highly configurable in order to show you the information you want and hide the information you don't.

### Configuring Lints in the `.flowconfig` {#toc-configuring-lints-in-the-flowconfig}

Lint settings can be specified in the `.flowconfig` [lints] section as a list of `rule=severity` pairs. These settings apply globally to the entire project.

**Example:**
```
[lints]
# all=off by default
all=warn
untyped-type-import=error
sketchy-null-bool=off
```

### Configuring Lints from the CLI {#toc-configuring-lints-from-the-cli}

Lint settings can be specified using the `--lints` flag of a Flow server command as a comma-delimited list of `rule=severity` pairs. These settings apply globally to the entire project.

**Example:**
```
flow start --lints "all=warn, untyped-type-import=error, sketchy-null-bool=off"
```

### Configuring Lints with Comments {#toc-configuring-lints-with-comments}

Lint settings can be specified inside a file using `flowlint` comments. These
settings apply to a region of a file, or a single line, or part of a line. For
more details see [Flowlint Comments](./flowlint-comments).

**Example:**
```js
 /* flowlint
  *   sketchy-null:error,
  *   untyped-type-import:error
  */
const x: ?number = 0;

if (x) {} // Error
import type {Foo} from './untyped.js'; // Error

// flowlint-next-line sketchy-null:off
if (x) {} // No Error

if (x) {} /* flowlint-line sketchy-null:off */ // No Error

// flowlint sketchy-null:off
if (x) {} // No Error
if (x) {} // No Error
import type {Bar} from './untyped.js'; // Error; unlike a $FlowFixMe, a flowlint comment only suppresses one particular type of error.
// flowlint sketchy-null:error
```

### Lint Settings Precedence {#toc-lint-settings-precedence}

Lint settings in `flowlint` comments have the highest priority, followed by lint rules in the `--lints` flag, followed by the `.flowconfig`.
This order allows you to use `flowlint` comments for fine-grained linting control, the `--lints` flag for trying out new lint settings, and the `.flowconfig` for stable project-wide settings.

Within the -lints flag and the flowconfig, rules lower down override rules higher up, allowing you to write things like
```
[lints]
sketchy-null=warn
sketchy-null-bool=off
```

The lint settings parser is fairly intelligent and will stop you if you write a redundant rule, a rule that gets completely overwritten, or an unused suppression. This should prevent most accidental misconfigurations of lint rules.

### Severity Levels and Meanings {#toc-severity-levels-and-meanings}

**off:**
The lint is ignored. Setting a lint to `off` is similar to suppressing a type error with a suppression comment, except with much more granularity.

**warn:**
Warnings are a new severity level introduced by the linting framework. They are treated differently than errors in a couple of ways:
* Warnings don't affect the exit code of Flow. If Flow finds warnings but no errors, it still returns 0.
* Warnings aren't shown on the CLI by default, to avoid spew. CLI warnings can be
    enabled by passing the --include-warnings flag to the Flow server or the
    Flow client, or by setting "include_warnings=true" in the `.flowconfig`.
    This is good for smaller projects that want to see all project warnings at once.
* Warnings have special [IDE Integration](./ide-integration).

**error:**
Lints with severity `error` are treated exactly the same as any other Flow error.
