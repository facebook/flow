---
layout: guide
---

To bring further consistency when working with Flow, you can integrate the [flowtype](https://github.com/gajus/eslint-plugin-flowtype) plugin with [eslint](https://github.com/eslint/eslint).

With eslint setup, you just need to install the plugin with babel-parser and add it to your eslint config.

```sh
yarn add --dev eslint-plugin-flowtype babel-eslint
# or
npm install --save-dev eslint-plugin-flowtype babel-eslint
```

```json
{
  "parser": "babel-eslint",
  "plugins": ["flowtype"],
  "extends": ["plugin:flowtype/recommended"],
  "settings": {
    "flowtype": {
      "onlyFilesWithFlowAnnotation": true
    }
  },
  "rules": {
    // ...rules
  }
}
```

> Now you can define consistency rules such as `delimiter-dangle` and `semi` or enforce partial strict syntax with `no-weak-types`
