# Flow

Flow is a static typechecker for Javascript. To find out more about Flow, check out [flowtype.org](http://flowtype.org/).

## Requirements

Flow works with:
* Mac OS X
* Linux (64-bit)
There are binary distributions for Mac OS X and many variants of Linux; you can also build it from source on almost any 64-bit Linux variant.

## Building Flow

Flow is written in OCaml (OCaml 4.x is required) and (on Linux) requires libelf. You can install OCaml on Mac OS X and Linux by following the instructions at [ocaml.org](https://ocaml.org/docs/install.html). 

Once you have these dependencies, building Flow just requires running

```
make
```

This produces a `bin` folder containing the `flow` binary. 

## Installing Flow

Flow is simple to install: all you need is the `flow` binary on your PATH and you're good to go. 

## Documentation

Check out http://flowtype.org for documentation and examples. 

## Join the Flow community
* Website: [http://flowtype.org/](http://flowtype.org/)
* irc: #flowtype on Freenode
* Twitter: @flowtype

## License
Flow is BSD-licensed. We also provide an additional patent grant.
