---
layout: guide
---

To bring further consistency when working with Flow, you can integrate the [flowtype](https://github.com/gajus/eslint-plugin-flowtype) plugin with eslint.

With eslint setup, you just need to install the plugin and add it to your eslint config

```sh
yarn add --dev eslint-plugin-flowtype
# or
npm install --save-dev eslint-plugin-flowtype
```

```json
{
  "plugins": ["flowtype"],
  "extends": ["plugin:flowtype/recommended"],
  "settings": {
    "flowtype": {
      "onlyFilesWithFlowAnnotation": true
    }
  }
}
```

> Now you can define consistency rules such as `delimiter-dangle` and `semi` or enforce partial strict syntax with `no-weak-types`
