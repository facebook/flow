# Flow [![Build Status](https://circleci.com/gh/facebook/flow/tree/master.svg?style=shield)](https://circleci.com/gh/facebook/flow/tree/master) [![Windows Build Status](https://ci.appveyor.com/api/projects/status/thyvx6i5nixtoocm/branch/master?svg=true)](https://ci.appveyor.com/project/Facebook/flow/branch/master) [![Join the chat at https://discordapp.com/invite/8ezwRUK](https://img.shields.io/discord/539606376339734558.svg?label=discord&logo=discord&logoColor=white)](https://discordapp.com/invite/8ezwRUK)

Flow is a static typechecker for JavaScript. To find out more about Flow, check out [flow.org](https://flow.org/).

For a background on the project, please read [this overview](https://flow.org/en/docs/lang/).

## Contents

- [Requirements](#requirements)
- [Using Flow](#using-flow)
- [Using Flow's parser from JavaScript](#using-flows-parser-from-javascript)
- [Building Flow from source](#building-flow-from-source)
- [Join the Flow community](#join-the-flow-community)
- [License](#license)


## Requirements

Flow works with:

* macOS
* Linux (64-bit)
* Windows (64-bit, Windows 10 recommended)

There are [binary distributions](https://github.com/facebook/flow/releases) for each of these platforms and you can also build it from source on any of them as well.

## Using Flow

Check out the [installation instructions](https://flow.org/en/docs/install/), and then [how to get started](https://flow.org/en/docs/usage/).

## Using Flow's parser from JavaScript

While Flow is written in OCaml, its parser is available as a compiled-to-JavaScript module published to npm, named [flow-parser](https://www.npmjs.com/package/flow-parser). **Most end users of Flow
will not need to use this parser directly**, but JavaScript packages which make use of parsing
Flow-typed JavaScript can use this to generate Flow's syntax tree with annotated types attached.

## Building Flow from source

Flow is written in OCaml (OCaml 4.07.1 is required).

1. Install [`opam`](http://opam.ocaml.org):

  - Mac: `brew install opam`
  - Debian: `sudo apt-get install opam`
  - Other Linux: see [opam docs](http://opam.ocaml.org/doc/Install.html)
  - Windows: see [OCaml for Windows docs](https://fdopen.github.io/opam-repository-mingw/installation/)

2. Initialize `opam`:

  ```sh
  opam init
  ```

3. Install OCaml and Flow's dependencies:

  ```sh
  # from within this git checkout
  opam switch create . --deps-only -y
  ```

4. Build the `flow` binary:

  ```sh
  eval $(opam env)
  make
  ```

  This produces the `bin/flow` binary.

5. Build `flow.js` (optional):

  ```sh
  opam install -y js_of_ocaml.3.4.0
  make js
  ```

  This produces `bin/flow.js`.

  The Flow parser can also be compiled to JavaScript. [Read how here](src/parser/README.md).

## Running the tests

To run the tests, first compile flow using `make`. Then run `bash ./runtests.sh bin/flow`

There is a `make test` target that compiles and runs tests.

To run a subset of the tests you can pass a second argument to the `runtests.sh` file.

For example: `bash runtests.sh bin/flow class | grep -v 'SKIP'`

## Join the Flow community
* Website: [https://flow.org](https://flow.org/)
* Discord: https://discord.gg/8ezwRUK
* irc: #flowtype on Freenode
* Twitter: follow [@flowtype](https://twitter.com/flowtype) and [#flowtype](https://twitter.com/hashtag/flowtype) to keep up with the latest Flow news.
* Stack Overflow: Ask a question with the [flowtype tag](http://stackoverflow.com/questions/tagged/flowtype)

## License
Flow is MIT-licensed ([LICENSE](http://github.com/facebook/flow/blob/master/LICENSE)). The [website](https://flow.org/) and [documentation](https://flow.org/en/docs/) are licensed under the Creative Commons Attribution 4.0 license ([website/LICENSE-DOCUMENTATION](https://github.com/facebook/flow/blob/master/website/LICENSE-DOCUMENTATION)).
