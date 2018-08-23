---
layout: guide
---

### flow-for-emacs <a class="toc" id="toc-flow-for-emacs" href="#toc-flow-for-emacs"></a>

You can add support for Flow in Emacs by using [flow-for-emacs](https://github.com/flowtype/flow-for-emacs)

### Requirements <a class="toc" id="toc-emacs-requirements" href="#toc-emacs-requirements"></a>

* Requires Flow to be installed and available on your path.
* Requires projects containing JavaScript files to be initialised with flow init.
* Requires JavaScript files to be marked with /* @flow */ at the top.

### Installation <a class="toc" id="toc-emacs-installation" href="#toc-emacs-installation"></a>

```sh
cd ~/.emacs.d/
git clone https://github.com/flowtype/flow-for-emacs.git
echo -e "\n(load-file \"~/.emacs.d/flow-for-emacs/flow.el\")" >> ~/.emacs
```