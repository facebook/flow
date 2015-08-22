# Flow

Flow is a static typechecker for JavaScript. To find out more about Flow, check out [flowtype.org](http://flowtype.org/).

For a background on the project, please read our [launch blog post](https://code.facebook.com/posts/1505962329687926/flow-a-new-static-type-checker-for-javascript/).

## Requirements

Flow works with:

* Mac OS X
* Linux (64-bit)

There are binary distributions for Mac OS X and many variants of Linux; you can also build it from source on almost any 64-bit Linux variant.

## Building Flow

Flow is written in OCaml (OCaml 4.01.0 or higher is required) and (on Linux) requires libelf. You can install OCaml on Mac OS X and Linux by following the instructions at [ocaml.org](https://ocaml.org/docs/install.html). 

For example, on Ubuntu 14.04 and similar systems:

```
sudo apt-get install ocaml libelf-dev
```

Once you have these dependencies, building Flow just requires running

```
make
```

This produces a `bin` folder containing the `flow` binary. 

*Note: at this time, the OCaml dependency prevents us from adding Flow to [npm](http://npmjs.org). Try [flow-bin](https://www.npmjs.org/package/flow-bin) if you need a npm binary wrapper.*

## Running the tests

To run the tests, first compile flow using `make`. Then run `bash ./runtests.sh bin/flow`

There is a `make test` target that compiles and runs tests.

To run a subset of the tests you can pass a second argument to the `runtests.sh` file.

For example: `bash runtests.sh bin/flow class | grep -v 'Skipping directory'`

## Installing Flow

Flow is simple to install: all you need is the `flow` binary on your PATH and you're good to go. 

### Using Homebrew

Installing Flow with [Homebrew](http://brew.sh/) package manager:

```
brew install flow
```

### Using OPAM

You can also build and install flow via the OCaml [OPAM](https://opam.ocaml.org) package manager with one command:

```
opam install flowtype
```

If you don't have a new enough version of OCaml to compile Flow, you can also use OPAM to bootstrap a modern version.  Install OPAM via the [binary packages](http://opam.ocaml.org/doc/Install.html#InstallOPAMin2minutes) for your operating system and run:

```
opam init --comp=4.01.0
opam install flowtype
eval `opam config env`
flow --help
```

## Documentation

Check out http://flowtype.org for documentation and examples. 

## Join the Flow community
* Website: [http://flowtype.org/](http://flowtype.org/)
* irc: #flowtype on Freenode
* Twitter: follow [@flowtype](https://twitter.com/flowtype) and [#flowtype](https://twitter.com/hashtag/flowtype) to keep up with the latest Flow news.

## License
Flow is BSD-licensed. We also provide an additional patent grant.
