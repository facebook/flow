# Flow
<p align="center">
  <a href="https://circleci.com/gh/facebook/flow/tree/main">
    <img src="https://circleci.com/gh/facebook/flow/tree/main.svg?style=shield" alt="CircleCI" />
  </a>
  <a href="https://twitter.com/flowtype">
   <img src="https://img.shields.io/twitter/follow/flowtype?style=shield&logo=twitter&color=blue" alt="Follow @flowtype" />
  </a>
  <a href="https://github.com/facebook/flow/blob/main/LICENSE">
    <img alt="MIT License" src="https://img.shields.io/github/license/facebook/flow">
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

## Contents

- [Requirements](#requirements)
- [Using Flow](#using-flow)
- [Using Flow's parser from JavaScript](#using-flows-parser-from-javascript)
- [Building Flow from source](#building-flow-from-source)
- [Join the Flow community](#join-the-flow-community)
- [License](#license)


## Requirements

Flow works with:

* macOS (arm64)
* Linux (x86_64 and arm64)
* Windows (x86_64, Windows 10 recommended)

There are [binary distributions](https://github.com/facebook/flow/releases) for each of these platforms and you can also build it from source on any of them as well.

## Using Flow

Check out the [installation instructions](https://flow.org/en/docs/getting-started/#toc-installation), and then [the usage docs](https://flow.org/en/docs/getting-started/#toc-usage).

## Using Flow's parser from JavaScript

Flow's parser is available as a compiled-to-JavaScript module published to npm, named [flow-parser](https://www.npmjs.com/package/flow-parser). **Most end users of Flow
will not need to use this parser directly**, but JavaScript packages which make use of parsing
Flow-typed JavaScript can use this to generate Flow's syntax tree with annotated types attached.

## Building Flow from source

Flow is written in Rust. GitHub CI builds Flow from the `rust_port` workspace with nightly Rust.

1. Install system dependencies:

    - Install [Rust via rustup](https://rustup.rs/).
    - Install the nightly Rust toolchain:

      ```sh
      rustup toolchain install nightly
      ```

2. Build the Rust workspace:

    ```sh
    cd rust_port
    cargo +nightly build
    ```

3. Run the Rust tests:

    ```sh
    cargo +nightly test
    ```

4. Build an optimized `flow` binary:

    ```sh
    cargo +nightly build --release --bin flow_cli
    ```

5. Build `flow.js` (optional):

    The `flow.js` build also uses the Rust port. Install Emscripten, Node, Yarn, and a Rust nightly toolchain with the `wasm32-unknown-emscripten` target and `rust-src` component first. GitHub CI uses Emscripten 3.1.44 and nightly Rust for this build.

    ```sh
    rustup toolchain install nightly-2026-04-14 --target wasm32-unknown-emscripten --component rust-src
    RUSTUP_TOOLCHAIN=nightly-2026-04-14 make js FLOW_JS_IMPL=rust-wasm
    ```

    This produces `bin/flow.js`.

    To build a faster, larger local development version instead, run:

    ```sh
    RUSTUP_TOOLCHAIN=nightly-2026-04-14 make js FLOW_JS_IMPL=rust-wasm FLOW_DOT_JS_WASM_PROFILE=dev
    ```

    The Flow parser can also be compiled to JavaScript. [Read how here](src/parser/README.md).

## Running the tests

See [tests/README.md](tests/README.md) for detailed testing documentation.

## Join the Flow community
* Website: [https://flow.org](https://flow.org/)
* Discord: https://discord.gg/8ezwRUK
* irc: #flowtype on Freenode
* Twitter: follow [@flowtype](https://twitter.com/flowtype) and [#flowtype](https://twitter.com/hashtag/flowtype) to keep up with the latest Flow news.
* Stack Overflow: Ask a question with the [flowtype tag](https://stackoverflow.com/questions/tagged/flowtype)

## License
Flow is MIT-licensed ([LICENSE](https://github.com/facebook/flow/blob/main/LICENSE)). The [website](https://flow.org/) and [documentation](https://flow.org/en/docs/) are licensed under the Creative Commons Attribution 4.0 license ([website/LICENSE-DOCUMENTATION](https://github.com/facebook/flow/blob/main/website/LICENSE-DOCUMENTATION)).
