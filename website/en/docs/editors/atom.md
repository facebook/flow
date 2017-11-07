---
layout: guide
---

If you're using [Atom](https://atom.io) you have a bunch of options to
integrate Flow into your code base.

### Atom IDE <a class="toc" id="toc-atom-ide-flowtype" href="#toc-atom-ide-flowtype"></a>

```sh
apm install atom-ide-ui ide-flowtype
```

Combining [atom-ide-ui](https://atom.io/packages/atom-ide-ui)
and [ide-flowtype](https://atom.io/packages/ide-flowtype), this is [Flow's
integration](https://nuclide.io/blog/2017/09/12/Introducing-Atom-IDE-UI/)
into the [Atom IDE](https://ide.atom.io/) initiative.
Like Nuclide, it’s a unified package which contains the following features:
* Diagnostics
* Definitions
* Find References
* Outline View
* Datatips
* Code Formatting
* Code Actions
* Code Highlight
* Busy Signal

Note that if you’re already a Nuclide user, all of Atom IDE UI’s features
will still be bundled inside of Nuclide, so there’s no need to install another
package.

### Nuclide <a class="toc" id="toc-nuclide" href="#toc-nuclide"></a>

```sh
apm install nuclide
```

[Nuclide](https://nuclide.io) is a full IDE created by people at Facebook that
has support for Flow built-in. It provides a linter, autocomplete and type
coverage support, click-to-definition and type description on hover.

However, it currently lacks support for on-the-fly type-checking (showing your
type errors before you save your file).

Nuclide also comes with many other features including support for remote
projects, hack, mercurial etc.

### Flow-IDE <a class="toc" id="toc-flow-ide" href="#toc-flow-ide"></a>

```sh
apm install flow-ide
```

[Flow-IDE](https://atom.io/packages/flow-ide) is a smaller package that only
provides you with a linter and autocomplete functionality. It, too, currently
lacks support for on-the-fly linting.

### Linter-Flow <a class="toc" id="toc-linter-flow" href="#toc-linter-flow"></a>

```sh
apm install linter-flow
```

In case you're looking for something even more minimal,
[linter-flow](https://atom.io/packages/linter-flow) may be worth your
attention. It only lints your code and provides no other features, but it does
support on-the-fly linting.

### Autocomplete-Flow <a class="toc" id="toc-autocomplete-flow" href="#toc-autocomplete-flow"></a>

```sh
apm install autocomplete-flow
```

[autocomplete-flow](https://atom.io/packages/autocomplete-flow) is another
purpose-built tool that only does one thing. This package, as the name
suggests, will give your flow enabled code autocomplete suggestions and nothing
else.
