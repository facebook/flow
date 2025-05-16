# Changelog

### 3.1.1 (2025-05-16)

#### Features

* Fix https://github.com/facebook/flow/issues/9281

### 3.1.0 (2025-03-31)

#### Features

* The extension now highlights refined values in green background. This can be disabled by `"flow.semanticDecorations": false`.
* Provides a way to see detailed errors like those from the CLI. You can either click on `See full errors` in the error message on hover, or trigger the code action "See detailed error" at the error location.

### 3.0.0 (2024-12-19)

* Bumped bundled flow-bin version.
* Unbreak extension for Windows machines. ([#9242](https://github.com/facebook/flow/pull/9242))
* Rewritten the extension in TypeScript to better keep up with upstream changes, because the upstream VSCode API typings are in TypeScript.

### [2.2.1](https://github.com/flowtype/flow-for-vscode/compare/2.2.0...2.2.1) (2023-01-06)

* Fix a security vulnerability.

### [2.2.0](https://github.com/flowtype/flow-for-vscode/compare/2.1.0...2.2.0) (2022-07-26)

* Removed support for the `log.file` flowconfig option, since it contained a security vulnerability.
* Upgraded bundled version to 0.183.0

### [2.1.0](https://github.com/flowtype/flow-for-vscode/compare/2.0.0...2.1.0) (2022-03-15)

* Prevent running versions of flow-bin < 0.110.0 in Restricted Mode, since those versions contained a security vulnerability.
  * Upgraded bundled version to 0.110.1
  * Removed support for flow-bin < 0.110.0 in Restricted Mode

### [2.0.0](https://github.com/flowtype/flow-for-vscode/compare/1.9.2...2.0.0) (2022-02-09)

* Removed support for legacy Flow integration. All communication with Flow now uses the Language Server Protocol.
* The "Path To Flow" setting is now ignored in Restricted Mode.

### [1.9.2](https://github.com/flowtype/flow-for-vscode/compare/1.9.1...1.9.2) (2021-09-07)

#### Bug Fixes

* **windows**: fix verification of `flow-bin` binaries ([9885ab6](https://github.com/flowtype/flow-for-vscode/commit/9885ab680b3f4ca8cc10ccb602fc7b6f9c1c125f))

### [1.9.1](https://github.com/flowtype/flow-for-vscode/compare/1.9.0...1.9.1) (2021-07-12)

#### Bug Fixes

* restore compatibility with older versions of VS Code ([00a4ff0](https://github.com/flowtype/flow-for-vscode/commit/00a4ff0171b751f8c000541dc352d1b28ba68470))

### [1.9.0](https://github.com/flowtype/flow-for-vscode/compare/1.5.0...1.9.0) (2021-06-29)

#### Features

* Verify integrity of `flow-bin` binaries ([af92850](https://github.com/flowtype/flow-for-vscode/commit/af92850998d2f3121ec475e6eaff5af163d9d301))
* Allow clickable links on hover for type descriptions ([1a43503](https://github.com/flowtype/flow-for-vscode/commit/1a435034b519df8d985da496dfee0cdc89dc377f))

### [1.5.0](https://github.com/flowtype/flow-for-vscode/compare/1.4.0...1.5.0) (2019-09-09)

#### Bug Fixes

* **windows:** closes [#362](https://github.com/flowtype/flow-for-vscode/issues/362) ([f1879e1](https://github.com/flowtype/flow-for-vscode/commit/f1879e1))

#### Features

* **flow-bin:** update bundled flow to v0.107.0 ([68f9ccf](https://github.com/flowtype/flow-for-vscode/commit/68f9ccf))

### [1.4.0](https://github.com/flowtype/flow-for-vscode/compare/1.3.0...1.4.0) (2019-09-02)

#### Bug Fixes

* **windows:** fix `flow is running on a different directory` ([@Mayank1791989](https://github.com/Mayank1791989) in [cd0831b](https://github.com/flowtype/flow-for-vscode/commit/cd0831b)), closes [#260](https://github.com/flowtype/flow-for-vscode/issues/260)

#### Features

* **flow-bin:** update bundled flow to v0.106.3 ([a4389fc](https://github.com/flowtype/flow-for-vscode/commit/a4389fc))

### [1.3.0](https://github.com/flowtype/flow-for-vscode/compare/1.2.0...1.3.0) (2019-08-04)

#### Bug Fixes

* **remote:** fix plugin not working with remote extension pack on windows ([@Mayank1791989](https://github.com/Mayank1791989) in [3dd4ecc](https://github.com/flowtype/flow-for-vscode/commit/3dd4ecc)), closes [#336](https://github.com/flowtype/flow-for-vscode/issues/336)

#### Features

* **flow-bin:** update bundled flow to v0.104.0 ([@Mayank1791989](https://github.com/Mayank1791989) in [5c15751](https://github.com/flowtype/flow-for-vscode/commit/5c15751))

### 1.2.0 (July 20, 2019)

#### Features
* **flow-bin:** update bundled flow to v0.103.0 ([@Mayank1791989](https://github.com/Mayank1791989) in [8d50b98](https://github.com/flowtype/flow-for-vscode/commit/8d50b98))

#### Bug Fixes
* **lsp:** fix issue [#350](https://github.com/flowtype/flow-for-vscode/issues/350) ([@Mayank1791989](https://github.com/Mayank1791989) in [ec9c9e0](https://github.com/flowtype/flow-for-vscode/commit/ec9c9e0))

### 1.1.4 (June 22, 2019)

* **build:** fix issue [#341](https://github.com/flowtype/flow-for-vscode/issues/341) ([@Mayank1791989](https://github.com/Mayank1791989) in [c6bacf0](https://github.com/flowtype/flow-for-vscode/commit/c6bacf0))
* **lsp:** accept flow release candidate version ([@telaoumatenyanis](https://github.com/telaoumatenyanis) in [#344](https://github.com/flowtype/flow-for-vscode/pull/344))

### 1.1.3 (June 12, 2019)

* Update bundled Flow to v0.101.0. ([@Mayank1791989](https://github.com/Mayank1791989) in [#340](https://github.com/flowtype/flow-for-vscode/pull/340))
* Update Node, Babel, Prettier, and setup Jest for testing. ([@Mayank1791989](https://github.com/Mayank1791989) in [#340](https://github.com/flowtype/flow-for-vscode/pull/340))

### 1.1.2 (June 11, 2019)

* Add `useCodeSnippetOnFunctionSuggest`. ([@Mayank1791989](https://github.com/Mayank1791989) in [#337](https://github.com/flowtype/flow-for-vscode/pull/337))
* Change `useNPMPackagedFlow` default to true. ([@Mayank1791989](https://github.com/Mayank1791989) in [#337](https://github.com/flowtype/flow-for-vscode/pull/337))
* Fix bundle might be bigger than expected ([#327](https://github.com/flowtype/flow-for-vscode/issues/327)). ([@Mayank1791989](https://github.com/Mayank1791989) in [#337](https://github.com/flowtype/flow-for-vscode/pull/337))
* Fix Intelllisense can't pick up functions in plain objects properly ([#335](https://github.com/flowtype/flow-for-vscode/issues/335)). ([@Mayank1791989](https://github.com/Mayank1791989) in [#337](https://github.com/flowtype/flow-for-vscode/pull/337))
* Fix restart vs. restart client ([#323](https://github.com/flowtype/flow-for-vscode/issues/323)). ([@Mayank1791989](https://github.com/Mayank1791989) in [#337](https://github.com/flowtype/flow-for-vscode/pull/337))

### 1.1.1 (March 30, 2019)

* Fix extension activates even for non-Flow (and non-JS) projects. ([@Mayank1791989](https://github.com/Mayank1791989) in [#326](https://github.com/flowtype/flow-for-vscode/pull/326))

### 1.1.0 (February 27, 2019)

* Multiple config and multi-root workspace support - [@Mayank1791989](https://github.com/Mayank1791989) [#315](https://github.com/flowtype/flow-for-vscode/pull/315)

### 1.0.1

* Bug fix for infinite loop - [@Mayank1791989](https://github.com/Mayank1791989) [#305](https://github.com/flowtype/flow-for-vscode/pull/305)

### 1.0.0

* Converts Flow LSP support to be default, and moves to using the flow-lsp module as the base:
  - [@Mayank1791989](https://github.com/Mayank1791989) [#295](https://github.com/flowtype/flow-for-vscode/pull/295)

### 0.8.4

* A potential fix for killing flow processes
  - [@FDiskas](https://github.com/FDiskas) [#269](https://github.com/flowtype/flow-for-vscode/pull/269)

* Fix go to definition for namespaced imports
  - [@minedeljkovic](https://github.com/minedeljkovic) [#265](https://github.com/flowtype/flow-for-vscode/pull/265)

### 0.8.2

* Updates for the flow language-server-plugin
  - [@thymikee](https://github.com/thymikee) [#258](https://github.com/flowtype/flow-for-vscode/pull/258)

* Add `showUncovered` config option to activate uncovered code report by default
  - [@HugoLime](https://github.com/HugoLime) [#256](https://github.com/flowtype/flow-for-vscode/pull/150)

### 0.8.1

* Adds windows support for the flow language-server-plugin
  - [@jbachhardie](https://github.com/jbachhardie) [#239](https://github.com/flowtype/flow-for-vscode/pull/239)

### 0.8.0

* Adds experimental support for flow language-server-plugin, to use it set `flow.useLSP` to true in your project/global settings
  - [@thymikee](https://github.com/thymikee) [#150](https://github.com/flowtype/flow-for-vscode/pull/150)
* Bump Flow to 0.68
  - [@thymikee](https://github.com/thymikee) [#221](https://github.com/flowtype/flow-for-vscode/pull/221)

### 0.7.3

* Improves `flow.pathToFlow` to allow passing in the workspace root `"${workspaceRoot}/node_modules/.bin/flow"`)
  - [@rattrayalex](https://github.com/rattrayalex) [#173](https://github.com/flowtype/flow-for-vscode/pull/173)

### 0.7.2

* Fixes for flow not re-checking on file changes- [@clintwood][]
* Add more file extensions for flow support - [@clintwood][]

### 0.7.1
* Adds a `flow.runOnAllFiles` option, which treats all files as if they had
  `// @flow` at the top. - [@doshisid](https://github.com/doshisid) [#144](https://github.com/flowtype/flow-for-vscode/pull/144)

### 0.7.0

* Bundles `flow-bin` with the plugin to use in cases where it has not been
  installed globally or locally. - [@BrainMaestro][] [#118](https://github.com/flowtype/flow-for-vscode/pull/118)
* Improvements around flow file detection, and reducing the amount of duplicate requests to the flow server. - [@thymikee][] [#126](https://github.com/flowtype/flow-for-vscode/pull/126)

### 0.6.0

* Adds the status indicator (spinner) to the statusbar which appears when flow is
  type checking, so that users can tell if everything type checked or if flow is
  just not finished yet. Indicator can be disabled by setting `flow.showStatus` to
  `false`. - [@gozala][] [#85](https://github.com/flowtype/flow-for-vscode/pull/85)
* Type checks code as you type. Only unsaved changes in an active document are
  considered (other unsaved documents are type checked by content on the disk).
  This feature can be disabled by setting `flow.runOnEdit` to `false` - [@gozala][]
  [#87](https://github.com/flowtype/flow-for-vscode/pull/87)
* Adds the [flowtype.org/try](http://flowtype.org/try/) like functionality to allow
  sketching, without setting up a project. Unsaved documents (ones created by
  `Files: New Untitled File` command) in `javascript` / `javascriptreact` mode and
  [`@flow` pragma](https://flowtype.org/docs/new-project.html#typechecking-your-files)
  will be typechecked as you type (assuming `flow.runOnEdit` is set to `true`). Please
  note that once file is saved, unless it's under project with `.flowconfig` it will
  no longer be type checked un further edits. - [@gozala]
  [#88](https://github.com/flowtype/flow-for-vscode/pull/88)
* Adds Flow type coverage indicator, allowing to also toggle diagnosis of code uncovered by flow by clicking on it. – [@thymikee][] [#108](https://github.com/flowtype/flow-for-vscode/pull/108)
* Redundant `[flow]` annotation is now removed. – [@thymikee][] [#106](https://github.com/flowtype/flow-for-vscode/pull/106)

### 0.5.0

* Uses the absolute path to the `where` command provided by Windows - [@JPanneel][] [#70](https://github.com/flowtype/flow-for-vscode/pull/70)
* Uses the new VS Code autocompletion API for functions - [@orta][] [#51](https://github.com/flowtype/flow-for-vscode/pull/51)
* When you want to work on the flow-for-vscode project, pressing run will start the
  babel build watcher task - [@orta][]
* Adds support for *.js.flow files ( such as those generated by graphql-js ) which are
  treated as common JavaScript files - [@orta][]
* Fixes "File not found" error with diagnostics when `flow status --json` provides
  non-absolute file paths - [@ryanashcraft][] [#77](https://github.com/flowtype/flow-for-vscode/pull/77)

### 0.4.0

* Adds the ability to use flow from your project's node_modules folder.
  This is a security risk ( see https://github.com/facebook/nuclide/issues/570) and so it
  is hidden behind a user setting of `useNPMPackagedFlow` which needs to be set to `true`
  for it to work. - [@orta][] [#53](https://github.com/flowtype/flow-for-vscode/pull/53)
* Show flow errors that start at line 0 - [@orta][] [#54](https://github.com/flowtype/flow-for-vscode/pull/54)

[@gozala]:https://github.com/Gozala
[@orta]:https://github.com/orta
[@thymikee]:https://github.com/thymikee
[@JPanneel]:https://github.com/JPanneel
[@ryanashcraft]:https://github.com/ryanashcraft
[@BrainMaestro]:https://github.com/BrainMaestro
[@clintwood]:https://github.com/clintwood
