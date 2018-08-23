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

### vim-flow <a class="toc" id="vim-flow" href="#vim-flow"></a>

Another way to add support for Flow in Vim is to use [vim-flow](https://github.com/flowtype/vim-flow).

* Adds completions to omnifunc
* Checks JavaScript files for type errors on save

#### Requirements <a class="toc" id="vim-requirements" href="#vim-requirements"></a>

* Requires Flow to be installed and available on your path.
* Requires projects containing JavaScript files to be initialised with flow init.
* Requires JavaScript files to be marked with /* @flow */ at the top.

#### Pathogen <a class="toc" id="pathogen" href="#pathogen"></a>

```sh
cd ~/.vim/bundle
git clone git://github.com/flowtype/vim-flow.git
```

#### NeoBundle <a class="toc" id="neobundle" href="#neobundle"></a>

Add this to your ~/.vimrc

```
  NeoBundleLazy 'flowtype/vim-flow', {
    \ 'autoload': {
    \     'filetypes': 'javascript'
    \ }}
```

With Flow build step, using flow-bin

```
  NeoBundleLazy 'flowtype/vim-flow', {
    \ 'autoload': {
    \     'filetypes': 'javascript'
    \ },
    \ 'build': {
    \     'mac': 'npm install -g flow-bin',
    \     'unix': 'npm install -g flow-bin'
    \ }}
```
