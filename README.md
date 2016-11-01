# Flow [![Build Status](https://travis-ci.org/facebook/flow.svg?branch=master)](https://travis-ci.org/facebook/flow) [![Windows Build Status](https://ci.appveyor.com/api/projects/status/thyvx6i5nixtoocm/branch/master?svg=true)](https://ci.appveyor.com/project/Facebook/flow/branch/master)

Flow is a static typechecker for JavaScript. To find out more about Flow, check out [flowtype.org](http://flowtype.org/).

For a background on the project, please read our [launch blog post](https://code.facebook.com/posts/1505962329687926/flow-a-new-static-type-checker-for-javascript/).

## Requirements

Flow works with:

* Mac OS X
* Linux (64-bit)
* Windows (64-bit)

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

On Mac OS X, you can install Flow via the [Homebrew](http://brew.sh/) package manager:

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
opam init --comp=4.03.0
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

- Run and see the magic happen
```
flow check
```

More thorough documentation and many examples can be found at http://flowtype.org.

## Building Flow

Flow is written in OCaml (OCaml 4.01.0 or higher is required) and (on Linux) requires libelf. You can install OCaml on Mac OS X and Linux by following the instructions at [ocaml.org](https://ocaml.org/docs/install.html).

For example, on Ubuntu 14.04 and similar systems:

```
sudo apt-get install ocaml libelf-dev
```

On OS X, using the [brew package manager](http://brew.sh/):

```
brew install ocaml ocamlbuild libelf opam
```

Once you have these dependencies, building Flow just requires running

```
make
```

This produces a `bin` folder containing the `flow` binary.

*Note: at this time, the OCaml dependency prevents us from adding Flow to [npm](http://npmjs.org). Try [flow-bin](https://www.npmjs.org/package/flow-bin) if you need a npm binary wrapper.*

Flow can also compile its parser to JavaScript. [Read how here](src/parser/README.md).

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
* Website: [http://flowtype.org/](http://flowtype.org/)
* irc: #flowtype on Freenode
* Twitter: follow [@flowtype](https://twitter.com/flowtype) and [#flowtype](https://twitter.com/hashtag/flowtype) to keep up with the latest Flow news.
* Stack Overflow: Ask a question with the [flowtype tag](http://stackoverflow.com/questions/tagged/flowtype)

## License
Flow is BSD-licensed. We also provide an additional patent grant.
