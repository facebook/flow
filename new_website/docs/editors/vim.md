---
title: Vim
slug: /editors/vim
---

Flow's editor integration is primarily via the [Language Server Protocol](https://microsoft.github.io/language-server-protocol/). There are [many vim LSP clients](https://microsoft.github.io/language-server-protocol/implementors/tools/) to choose from, such as [ALE](#toc-ale).

Alternatively, the legacy [vim-flow](#toc-vim-flow) extension is Flow-specific, and provides fewer features.

## ALE {#toc-ale}

The Asynchronous Lint Engine (ALE) plugin for Vim 8+ and NeoVim, [vim-ale](https://github.com/w0rp/ale), is a generalized linting engine with support for Flow and many other tools.

### Installation {#toc-installation}

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

## coc.nvim-neovim {#toc-coc-nvim-neovim}

[Coc](https://github.com/neoclide/coc.nvim) is an intellisense engine for vim8 & neovim.

### Setup {#toc-setup}

```vim
set nocompatible
filetype off

" install coc.nvim using Plug or preffered plugin manager
call plug#begin('~/.vim/plugged')
Plug 'neoclide/coc.nvim', {'branch': 'release'}
call plug#end()

filetype plugin indent on

" ======= coc settings
set updatetime=300
set shortmess+=c

" Use leader T to show documentation in preview window
nnoremap <leader>t :call <SID>show_documentation()<CR>


function! s:show_documentation()
  if (index(['vim','help'], &filetype) >= 0)
    execute 'h '.expand('&lt;cword&gt;')
  else
    call CocAction('doHover')
  endif
endfunction

" instead of having ~/.vim/coc-settings.json
let s:LSP_CONFIG = {
\  'flow': {
\    'command': exepath('flow'),
\    'args': ['lsp'],
\    'filetypes': ['javascript', 'javascriptreact'],
\    'initializationOptions': {},
\    'requireRootPattern': 1,
\    'settings': {},
\    'rootPatterns': ['.flowconfig']
\  }
\}

let s:languageservers = {}
for [lsp, config] in items(s:LSP_CONFIG)
  let s:not_empty_cmd = !empty(get(config, 'command'))
  if s:not_empty_cmd | let s:languageservers[lsp] = config | endif
endfor

if !empty(s:languageservers)
  call coc#config('languageserver', s:languageservers)
  endif
```

## LanguageClient-neovim {#toc-languageclient-neovim}

Another way to add support for Flow in Vim is to use [LanguageClient-neovim](https://github.com/autozimu/LanguageClient-neovim).

* Suports vim 8 and neovim
* Adds completions to omnifunc
* Checks JavaScript files for type errors on save
* Look up types under cursor

### Requirements {#toc-requirements}

* Requires Flow to be installed and available on your path.
* Requires projects containing JavaScript files to be initialised with flow init.
* Requires JavaScript files to be marked with /* @flow */ at the top.

### Pathogen {#toc-pathogen}

```sh
cd ~/.vim/bundle
git clone git://github.com/autozimu/LanguageClient-neovim.git
```

### NeoBundle {#toc-neobundle}

Add this to your ~/.vimrc

```vim
  NeoBundleLazy 'autozimu/LanguageClient-neovim', {
    \ 'autoload': {
    \     'filetypes': 'javascript'
    \ }}
```

With Flow build step, using flow-bin

```vim
  NeoBundleLazy 'autozimu/LanguageClient-neovim', {
    \ 'autoload': {
    \     'filetypes': 'javascript'
    \ },
    \ 'build': {
    \     'mac': 'npm install -g flow-bin',
    \     'unix': 'npm install -g flow-bin'
    \ }}
```

### VimPlug {#toc-vimplug}

```vim
  Plug 'autozimu/LanguageClient-neovim', {
    \ 'branch': 'next',
    \ 'do': 'bash install.sh && npm install -g flow-bin',
    \ }
```

### Setup {#toc-setup}
```vim
let g:LanguageClient_rootMarkers = {
\   'javascript': ['.flowconfig', 'package.json']
\ }
let g:LanguageClient_serverCommands={
\   'javascript': ['flow', 'lsp'],
\   'javascript.jsx': ['flow', 'lsp']
\}

" check the type under cursor w/ leader T
nnoremap <leader>t :call LanguageClient_textDocument_hover()<CR>
nnoremap <leader>y :call LanguageClient_textDocument_definition()<CR>
```

## vim-flow {#toc-vim-flow}

Another way to add support for Flow in Vim is to use [vim-flow](https://github.com/flowtype/vim-flow).

* Adds completions to omnifunc
* Checks JavaScript files for type errors on save

### Requirements {#toc-requirements}

* Requires Flow to be installed and available on your path.
* Requires projects containing JavaScript files to be initialised with flow init.
* Requires JavaScript files to be marked with /* @flow */ at the top.

### Pathogen {#toc-pathogen}

```sh
cd ~/.vim/bundle
git clone git://github.com/flowtype/vim-flow.git
```

### NeoBundle {#toc-neobundle}

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
