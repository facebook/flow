---
title: Emacs
slug: /editors/emacs
---

### flow-for-emacs {#toc-flow-for-emacs}

You can add support for Flow in Emacs by using [flow-for-emacs](https://github.com/flowtype/flow-for-emacs)

### Requirements {#toc-emacs-requirements}

* Requires Flow to be installed and available on your path.
* Requires projects containing JavaScript files to be initialised with flow init.
* Requires JavaScript files to be marked with /* @flow */ at the top.

### Installation {#toc-emacs-installation}

```sh
cd ~/.emacs.d/
git clone https://github.com/flowtype/flow-for-emacs.git
echo -e "\n(load-file \"~/.emacs.d/flow-for-emacs/flow.el\")" >> ~/.emacs
```
