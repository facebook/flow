# Flow [![Build Status](https://circleci.com/gh/facebook/flow/tree/master.svg?style=shield)](https://circleci.com/gh/facebook/flow/tree/master) [![Windows Build Status](https://ci.appveyor.com/api/projects/status/thyvx6i5nixtoocm/branch/master?svg=true)](https://ci.appveyor.com/project/Facebook/flow/branch/master) [![Join the chat at https://discordapp.com/invite/8ezwRUK](https://img.shields.io/discord/539606376339734558.svg?label=discord&logo=discord&logoColor=white)](https://discordapp.com/invite/8ezwRUK)

Flow is a static typechecker for JavaScript. To find out more about Flow, check out [flow.org](https://flow.org/).

For a background on the project, please read [this overview](https://flow.org/en/docs/lang/).

## Requirements

Flow works with:

* macOS
* Linux (64-bit)
* Windows (64-bit, Windows 10 recommended)

There are [binary distributions](https://github.com/facebook/flow/releases) for each of these platforms and you can also build it from source on any of them as well.

## Installing Flow

Flow is simple to install: all you need is the `flow` binary on your PATH and you're good to go.


### Installing Flow Per Project

The recommended way to install Flow is via the [`flow-bin`](https://www.npmjs.com/package/flow-bin) `npm` package. Adding `flow-bin` to your project's `package.json`:

- provides a smoother upgrade experience, since the correct version of Flow is automatically used based on the revision you check out
- installs Flow as part of your existing `npm install` workflow
- lets you use different versions of Flow on different projects

```
npm install --save-dev flow-bin
node_modules/.bin/flow
```

### Installing Flow Globally

Although not recommended, you can also install Flow globally (for example, perhaps you don't use `npm` or `package.json`).

The best way to install globally is via `flow-bin`:

```
npm install -g flow-bin
flow # make sure `npm bin -g` is on your path
```

On macOS, you can install Flow via the [Homebrew](http://brew.sh/) package manager:

```
brew update
brew install flow
```

You can also build and install Flow via the OCaml [OPAM](https://opam.ocaml.org) package manager. Since Flow has some non-OCaml dependencies, you need to use the [`depext`](https://opam.ocaml.org/doc/FAQ.html#Somepackagefailduringcompilationcomplainingaboutmissingdependenciesquotm4quotquotlibgtkquotetc) package like so:

```
opam install depext
opam depext --install flowtype
```

If you don't have a new enough version of OCaml to compile Flow, you can also use OPAM to bootstrap a modern version.  Install OPAM via the [binary packages](http://opam.ocaml.org/doc/Install.html#InstallOPAMin2minutes) for your operating system and run:

```
opam init --comp=4.05.0
opam install flowtype
eval `opam config env`
flow --help
```


## Getting started

Getting started with flow is super easy.

- Initialize Flow by running the following command in the root of your project
```
flow init
```

- Add the following to the top of all the files you want to typecheck
``` javascript
/* @flow */
```
or
``` javascript
// @flow
```

- Run and see the magic happen
```
flow check
```

More thorough documentation and many examples can be found at [flow.org](https://flow.org/).

## Building Flow

Flow is written in OCaml (OCaml 4.05.0 or higher is required). You can install OCaml on macOS and Linux by following the instructions at [ocaml.org](https://ocaml.org/docs/install.html).

For example, on Ubuntu 16.04 and similar systems:

```
sudo apt-get install opam
opam init --comp 4.05.0
```

On macOS, using the [brew package manager](http://brew.sh/):

```
brew install opam
opam init --comp 4.05.0
```

Then, restart your shell and install these additional libraries:

```
opam update
opam pin add flowtype . -n
opam install --deps-only flowtype
```

Once you have these dependencies, building Flow just requires running

```
make
```

This produces a `bin` folder containing the `flow` binary.

In order to make the flow.js file, you first need to install js_of_ocaml:

```
opam install -y js_of_ocaml
```

After that, making flow.js is easy:

```
make js
```

The new `flow.js` file will also live in the `bin` folder.

*Note: at this time, the OCaml dependency prevents us from adding Flow to [npm](http://npmjs.org). Try [flow-bin](https://www.npmjs.org/package/flow-bin) if you need a npm binary wrapper.*

Flow can also compile its parser to JavaScript. [Read how here](src/parser/README.md).

## Building Flow on Windows

This is a little more complicated. Here is a process that works, though it probably can be simplified.

The general idea is that we build in Cygwin, targeting mingw. This gives us a binary that works even outside of Cygwin.

### Install Cygwin
1. Install Cygwin 64bit from https://cygwin.com/install.html
2. In powershell, run `iex ((new-object net.webclient).DownloadString("https://raw.githubusercontent.com/ocaml/ocaml-ci-scripts/master/appveyor-install.ps1"))` which will likely run a cygwin setup installer with a bunch of cygwin packages and stuff. This helps make sure that every package that opam needs is available.

### Install Opam
1. Open the cygwin64 terminal
2. Download opam with `curl -fsSL -o opam64.tar.xz https://github.com/fdopen/opam-repository-mingw/releases/download/0.0.0.1/opam64.tar.xz`
3. `tar -xf opam64.tar.xz`
4. `cd opam64`
5. Install opam `./install.sh`
6. Initialize opam to point to a mingw fork: `opam init -a default "https://github.com/fdopen/opam-repository-mingw.git" --comp "4.05.0+mingw64c" --switch "4.05.0+mingw64c"`
7. Make sure opam stuff is in your path: ```eval `opam config env` ```

### Install Flow
1. Clone flow: `git clone https://github.com/facebook/flow.git`
2. `cd flow`
3. Tell opam to use this directory as the flowtype project: `opam pin add flowtype . -n`
4. Install system dependencies `opam depext -u flowtype`
5. Install Flow's dependencies `opam install flowtype --deps-only`
7. Finally, build Flow: `make all`

## Using Flow's parser from JavaScript

While Flow is written in OCaml, its parser is available as a compiled-to-JavaScript module published to npm, named [flow-parser](https://www.npmjs.com/package/flow-parser). **Most end users of Flow
will not need to use this parser directly** (and should install [flow-bin](https://www.npmjs.org/package/flow-bin) from npm above), but JavaScript packages which make use of parsing
Flow-typed JavaScript can use this to generate Flow's syntax tree with annotated types attached.

## Running the tests

To run the tests, first compile flow using `make`. Then run `bash ./runtests.sh bin/flow`

There is a `make test` target that compiles and runs tests.

To run a subset of the tests you can pass a second argument to the `runtests.sh` file.

For example: `bash runtests.sh bin/flow class | grep -v 'SKIP'`

## Join the Flow community
* Website: [https://flow.org](https://flow.org/)
* Discord (for contributors): https://discord.gg/8ezwRUK
* irc: #flowtype on Freenode
* Twitter: follow [@flowtype](https://twitter.com/flowtype) and [#flowtype](https://twitter.com/hashtag/flowtype) to keep up with the latest Flow news.
* Stack Overflow: Ask a question with the [flowtype tag](http://stackoverflow.com/questions/tagged/flowtype)

## License
Flow is MIT-licensed ([LICENSE](http://github.com/facebook/flow/blob/master/LICENSE)). The [website](https://flow.org/) and [documentation](https://flow.org/en/docs/) are licensed under the Creative Commons Attribution 4.0 license ([website/LICENSE-DOCUMENTATION](https://github.com/facebook/flow/blob/master/website/LICENSE-DOCUMENTATION)).
