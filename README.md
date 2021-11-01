# Flow
<p align="center">
  <a href="https://circleci.com/gh/facebook/flow/tree/master">
    <img src="https://circleci.com/gh/facebook/flow/tree/master.svg?style=shield" alt="CircleCI" />
  </a>
  <a href="https://twitter.com/flowtype">
   <img src="https://img.shields.io/twitter/follow/flowtype?style=social" alt="Follow @flowtype" />
  </a>
  <a href="https://github.com/facebook/flow/blob/master/LICENSE">
    <img alt="GitHub" src="https://img.shields.io/github/license/facebook/flow">
  </a>
  <a href="https://github.com/facebook/flow/graphs/contributors">
   <img alt="GitHub contributors" src="https://img.shields.io/github/contributors/facebook/flow">
  </a>
  <a href="">
    <img alt="GitHub top language" src="https://img.shields.io/github/languages/top/facebook/flow">
  </a>
  <a href="https://discordapp.com/invite/8ezwRUK">
     <img alt="Join Discord Chat" src="https://img.shields.io/discord/539606376339734558.svg?label=discord&logo=discord&logoColor=white">
  </a>
</p>


Flow is a static typechecker for JavaScript. To find out more about Flow, check out [flow.org](https://flow.org/).

For more background information on the project, please read [this overview](https://flow.org/en/docs/lang/).

## Contents

- [Requirements](#requirements)
- [Using Flow](#using-flow)
- [Using Flow's parser from JavaScript](#using-flows-parser-from-javascript)
- [Building Flow from source](#building-flow-from-source)
- [Join the Flow community](#join-the-flow-community)
- [License](#license)


## Requirements

Flow works with:

* macOS (x86_64)
* Linux (x86_64 and arm64)
* Windows (x86_64, Windows 10 recommended)

There are [binary distributions](https://github.com/facebook/flow/releases) for each of these platforms and you can also build it from source on any of them as well.

## Using Flow

Check out the [installation instructions](https://flow.org/en/docs/install/), and then [the usage docs](https://flow.org/en/docs/usage/).

## Using Flow's parser from JavaScript

While Flow is written in OCaml, its parser is available as a compiled-to-JavaScript module published to npm, named [flow-parser](https://www.npmjs.com/package/flow-parser). **Most end users of Flow
will not need to use this parser directly**, but JavaScript packages which make use of parsing
Flow-typed JavaScript can use this to generate Flow's syntax tree with annotated types attached.

## Building Flow from source

Flow is written in OCaml (OCaml 4.10.2 is required).

1. Install system dependencies:

    - Mac: `brew install opam`
    - Debian: `sudo apt-get install opam`
    - Other Linux: see [opam docs](https://opam.ocaml.org/doc/Install.html)
    - Windows: [cygwin](https://cygwin.com/) and a number of dependencies like `make`, `gcc` and `g++` are required.

      One way to install everything is to install [Chocolatey](https://chocolatey.org/) and then run `.\scripts\windows\install_deps.ps1` and `.\scripts\windows\install_opam.ps1`. Otherwise, see the "Manual Installation" section of [OCaml for Windows docs](https://fdopen.github.io/opam-repository-mingw/installation/) and install all of the packages listed in our `install_deps.ps1`.

      The remainder of these instructions should be run inside the Cygwin shell: `C:\tools\cygwin\Cygwin`. Then `cd /cygdrive/c/Users/you/path/to/checkout`.

2. Validate the `opam` version is `2.x.x`:

    ```sh
    opam --version
    ```

    The following instructions expect `2.x.x`. Should your package manager have installed a `1.x.x` version, please refer to the [opam docs](https://opam.ocaml.org/doc/Install.html) to install a newer version manually.

3. Initialize `opam`:

    ```sh
    # on Mac and Linux:
    opam init

    # on Windows:
    scripts/windows/init_opam.sh
    ```

4. Install Flow's OCaml dependencies:

    ```sh
    # from within this git checkout
    make deps
    ```

    **note**: If you find that you get an error looking for `ocaml-base-compiler` version, your local dependency repo may be out of date and you need to run `opam update` + `opam upgrade`

5. Build the `flow` binary:

    ```sh
    eval $(opam env)
    make
    ```

    This produces the `bin/flow` binary.

6. Build `flow.js` (optional):

    ```sh
    opam install -y js_of_ocaml.3.9.0
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
* Stack Overflow: Ask a question with the [flowtype tag](https://stackoverflow.com/questions/tagged/flowtype)

## License
Flow is MIT-licensed ([LICENSE](https://github.com/facebook/flow/blob/master/LICENSE)). The [website](https://flow.org/) and [documentation](https://flow.org/en/docs/) are licensed under the Creative Commons Attribution 4.0 license ([website/LICENSE-DOCUMENTATION](https://github.com/facebook/flow/blob/master/website/LICENSE-DOCUMENTATION)).
