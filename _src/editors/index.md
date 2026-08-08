---
title: Editors
description: "How to integrate Flow with your code editor."
slug: /editors
---

Flow's editor integration is provided through the [Language Server Protocol](https://microsoft.github.io/language-server-protocol/) (LSP). The `flow lsp` command starts Flow as an LSP server, so any editor with an LSP client can connect to it for diagnostics, autocomplete, go-to-definition, hover types, code actions, and more.

## Visual Studio Code

The officially maintained extension is [Flow Language Support](https://marketplace.visualstudio.com/items?itemName=flowtype.flow-for-vscode). Install it from the VS Code extensions panel or the Marketplace.

## Other editors

Any editor with LSP support can be configured to use Flow by pointing its LSP client at `flow lsp`. Consult your editor or LSP plugin's documentation for setup details.
