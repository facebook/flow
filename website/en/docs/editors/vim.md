---
layout: guide
---

### ALE <a class="toc" id="vim-ale" href="#vim-ale"></a>

The Asynchronous Lint Engine (ALE) [vim-ale](https://github.com/w0rp/ale) plugin for Vim 8+ and NeoVim provides linting of many syntaxes during editing, before files are saved. Supported JavaScript linters include eslint, jscs, jshint, flow, standard, and xo.  Installation instructions can be found at [https://github.com/w0rp/ale#3-installation](https://github.com/w0rp/ale#3-installation).

Here is suggested ALE configuration to place in your .vimrc file:
```
" Asynchronous Lint Engine (ALE)
" Limit linters used for JavaScript.
let g:ale_linters = {
\  'javascript': ['flow'] " can add more
\}
highlight clear ALEErrorSign " otherwise uses error bg color (typically red)
highlight clear ALEWarningSign " otherwise uses error bg color (typically red)
let g:ale_sign_error = 'X' " could use emoji
let g:ale_sign_warning = '?' " could use emoji
let g:ale_statusline_format = ['X %d', '? %d', '']
" %linter% is the name of the linter that provided the message
" %s is the error or warning message
let g:ale_echo_msg_format = '%linter% says %s'
" Map keys to navigate between lines with errors and warnings.
nnoremap <leader>an :ALENextWrap<cr>
nnoremap <leader>ap :ALEPreviousWrap<cr>
```

### coc.nvim-neovim <a class="toc" id="cocnvim" href="#cocnvim"></a>

[Coc](https://github.com/neoclide/coc.nvim) is an intellisense engine for vim8 & neovim.

### [Setup](https://gist.github.com/antonk52/4587b2687268a5dde494240ce975a708)

<details>
<summary>Minimal vimrc</summar>
<pre>
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
    execute 'h '.expand('<cword>')
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
</pre>
</details>

### LanguageClient-neovim <a class="toc" id="LanguageClient-neovim" href="#LanguageClient-neovim"></a>

Another way to add support for Flow in Vim is to use [LanguageClient-neovim](https://github.com/autozimu/LanguageClient-neovim).

* Suports vim 8 and neovim
* Adds completions to omnifunc
* Checks JavaScript files for type errors on save
* Look up types under cursor

#### Requirements <a class="toc" id="vim-requirements" href="#vim-requirements"></a>

* Requires Flow to be installed and available on your path.
* Requires projects containing JavaScript files to be initialised with flow init.
* Requires JavaScript files to be marked with /* @flow */ at the top.

#### Pathogen <a class="toc" id="pathogen" href="#pathogen"></a>

```sh
cd ~/.vim/bundle
git clone git://github.com/autozimu/LanguageClient-neovim.git
```

#### NeoBundle <a class="toc" id="neobundle" href="#neobundle"></a>

Add this to your ~/.vimrc

```
  NeoBundleLazy 'autozimu/LanguageClient-neovim', {
    \ 'autoload': {
    \     'filetypes': 'javascript'
    \ }}
```

With Flow build step, using flow-bin

```
  NeoBundleLazy 'autozimu/LanguageClient-neovim', {
    \ 'autoload': {
    \     'filetypes': 'javascript'
    \ },
    \ 'build': {
    \     'mac': 'npm install -g flow-bin',
    \     'unix': 'npm install -g flow-bin'
    \ }}
```

#### VimPlug <a class="toc" id="vimplug" href="#vimplug"></a>

```
  Plug 'autozimu/LanguageClient-neovim', {
    \ 'branch': 'next',
    \ 'do': 'bash install.sh && npm install -g flow-bin',
    \ }
```

#### Setup
```
let g:LanguageClient_rootMarkers = {
\   'javascript': ['tsconfig.json', '.flowconfig', 'package.json']
\ }
" auto start server for these file types
let g:LSP_ts_command = ['typescript-language-server', '--stdio']
let g:LanguageClient_serverCommands={
\   'javascript': ['flow', 'lsp'],
\   'javascript.jsx': ['flow', 'lsp']
\}

" check the type under cursor w/ leader T
nnoremap <leader>t :call LanguageClient_textDocument_hover()<CR>
nnoremap <leader>y :call LanguageClient_textDocument_definition()<CR>
```
