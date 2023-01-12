---
title: "Windows Support is Here!"
short-title: "Windows Support"
author: "Gabe Levi"
hide_table_of_contents: true
---

We are excited to announce that Flow is now officially available on 64-bit
Windows! Starting with
[Flow v0.30.0](https://github.com/facebook/flow/releases/tag/v0.30.0), we will
publish a Windows binary with each release. You can download the Windows binary
in a .zip file directly from the
[GitHub releases page](https://github.com/facebook/flow/releases) or install it
using the [flow-bin npm package](https://www.npmjs.com/package/flow-bin). Try
it out and [report any issues](https://github.com/facebook/flow/issues) you
come across!

![Windows Support GIF]({{ site.baseurl }}/static/windows.gif)

Getting Flow working on Windows was not easy, and it was made possible by the
hard work of [Grégoire](https://github.com/OCamlPro-Henry),
[Çagdas](https://github.com/OCamlPro-Bozman) and
[Fabrice](https://github.com/lefessan) from
[OCamlPro](https://www.ocamlpro.com/).

<!--truncate-->

### Getting Started with Windows

#### Getting Started with flow-bin on Windows

Does your JavaScript project use npm to manage dependencies? Well, then the
easiest way for you to install Flow is with npm! Just run

```bash
> npm install --save-dev flow-bin
```

(Note: on Windows, it is recommended to use npm v3, to avoid the long
`node_modules` paths that npm v2 creates)

This will install the
[flow-bin npm package](https://www.npmjs.com/package/flow-bin) and
automatically add it to your package.json. Once installed, there are a few ways
to use the Flow binary. One is to use `./node_modules/.bin/flow` directly. For
example, every Flow project needs a `.flowconfig` file in the root directory.
If you don't already have a `.flowconfig`, you could create it with Powershell,
like

```bash
> New-Item .flowconfig
```

or you could run the `flow init` command, using `./node_modules/.bin/flow`

```bash
> ./node_modules/.bin/flow init
```

Another way to run Flow is via an npm script. In your package.json file, there
is a `"scripts"` section. Maybe it looks like this:

```js
"scripts": {
  "test": "make test"
}
```

You can run the Flow binary directly from a script by referencing `flow` in a
script, like so:

```js
"scripts": {
  "test": "make test",
  "flow_check": "flow check || exit 0"
}
```

and then running that script via `npm run`

```bash
> npm run flow_check
```

(Note: the `|| exit 0` part of the script is optional, but `npm run` will show
an error message if the script finishes with a non-zero exit code)

You can also install `flow-bin` globally with

```bash
> npm install --global flow-bin
```

#### Getting Started with flow.exe

Each [GitHub release of Flow](https://github.com/facebook/flow/releases)
starting with v0.30.0 will have a zipped Windows binary. For example, the
[v0.30.0 release](https://github.com/facebook/flow/releases/tag/v0.30.0)
includes [flow-win64-v0.30.0.zip](https://github.com/facebook/flow/releases/download/v0.30.0/flow-win64-v0.30.0.zip).
If you download and unzip that, you will find a `flow/` directory, which
contains `flow.exe`. `flow.exe` is the Flow binary, so if you put that
somewhere in your path, and you should be good to go.

```bash
> mkdir demo
> cd demo
> flow.exe init
> "/* @flow */ var x: number = true;" | Out-File -Encoding ascii test.js
> flow.exe check
test.js:1
  1: /* @flow */ var x: number = true;
                                 ^^^^ boolean. This type is incompatible with

  1: /* @flow */ var x: number = true;
                        ^^^^^^ number
```
