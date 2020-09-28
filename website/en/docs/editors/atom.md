---
layout: guide
---

If you're using [Atom](https://atom.io) you have a bunch of options to
integrate Flow into your code base.

### Flow for Atom IDE <a class="toc" id="toc-ide-flowtype" href="#toc-ide-flowtype"></a>

```sh
apm install atom-ide-ui && apm install ide-flowtype
```

[Flow for Atom IDE](https://atom.io/packages/ide-flowtype) is extracted from
[Nuclide](https://nuclide.io), and works with the [Atom IDE](https://ide.atom.io/) UI. It brings
the core features you expect in a full-featured IDE into Atom, such as
language-aware autocomplete, diagnostics, go-to-definition, type hints, and
symbol outlines.

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
