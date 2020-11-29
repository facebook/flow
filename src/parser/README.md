# The Flow Parser

The Flow Parser is a JavaScript parser written in OCaml. It produces an AST that conforms to [SpiderMonkey's Parser API](https://developer.mozilla.org/en-US/docs/Mozilla/Projects/SpiderMonkey/Parser_API) and that mostly matches what [esprima](http://esprima.org/) produces. The Flow Parser can be compiled to native code or can be compiled to JavaScript using [js_of_ocaml](http://ocsigen.org/js_of_ocaml/).

## Building the Flow Parser

Building the Flow Parser requires OCaml. Compiling to JavaScript requires js_of_ocaml >= 3.6.

### Initial set up

* [Install opam](https://opam.ocaml.org/doc/Install.html)
* `opam install js_of_ocaml`

### Building the OCaml Flow Parser library

    make

### Compiling the Flow Parser to JavaScript

    make js

## Tests

The Flow Parser's test suite tests the JavaScript version of the parser, so you will need js_of_ocaml installed. The tests and tools also have some node module dependencies, so you will need to run

### Initial set up

* Follow the steps in [Building the Flow Parser](https://github.com/facebook/flow/blob/master/src/parser/README.md#building-the-flow-parser)
* `npm install`

### Running the Tests

    make test
