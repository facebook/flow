---
layout: guide
---

Flow's editor integration is primarily via the [Language Server Protocol](https://microsoft.github.io/language-server-protocol/). There are [many vim LSP clients](https://microsoft.github.io/language-server-protocol/implementors/tools/) to choose from, such as [ALE](#toc-ale).

Alternatively, the legacy [vim-flow](#toc-vim-flow) extension is Flow-specific, and provides fewer features.

## ALE <a class="toc" id="toc-ale" href="#toc-ale"></a>

The Asynchronous Lint Engine (ALE) plugin for Vim 8+ and NeoVim, [vim-ale](https://github.com/w0rp/ale), is a generalized linting engine with support for Flow and many other tools.

### Installation <a class="toc" id="toc-installation" href="#toc-installation"></a>

Follow the [instructions](https://github.com/w0rp/ale#3-installation) in the ALE README.

Configure ALE to use the `flow-language-server` linter for JavaScript files:

```vim
" In ~/.vim/ftplugin/javascript.vim, or somewhere similar.

" Enables only Flow for JavaScript. See :ALEInfo for a list of other available
" linters. NOTE: the `flow` linter uses an old API; prefer `flow-language-server`.
let b:ale_linters = ['flow-language-server']

" Or in ~/.vim/vimrc:
let g:ale_linters = {
\   'javascript': ['flow-language-server'],
\}
```

## vim-flow <a class="toc" id="toc-vim-flow" href="#toc-vim-flow"></a>

Another way to add support for Flow in Vim is to use [vim-flow](https://github.com/flowtype/vim-flow).

* Adds completions to omnifunc
* Checks JavaScript files for type errors on save

### Requirements <a class="toc" id="toc-requirements" href="#toc-requirements"></a>

* Requires Flow to be installed and available on your path.
* Requires projects containing JavaScript files to be initialised with flow init.
* Requires JavaScript files to be marked with /* @flow */ at the top.

### Pathogen <a class="toc" id="toc-pathogen" href="#toc-pathogen"></a>

```sh
cd ~/.vim/bundle
git clone git://github.com/flowtype/vim-flow.git
```

### NeoBundle <a class="toc" id="toc-neobundle" href="#toc-neobundle"></a>

Add this to your ~/.vimrc

```vim
  NeoBundleLazy 'flowtype/vim-flow', {
    \ 'autoload': {
    \     'filetypes': 'javascript'
    \ }}
```

With Flow build step, using flow-bin

```vim
  NeoBundleLazy 'flowtype/vim-flow', {
    \ 'autoload': {
    \     'filetypes': 'javascript'
    \ },
    \ 'build': {
    \     'mac': 'npm install -g flow-bin',
    \     'unix': 'npm install -g flow-bin'
    \ }}
```
