[flow-remove-types](https://github.com/facebook/flow/tree/master/packages/flow-remove-types) is a small
CLI tool for stripping Flow type annotations from files. It's a lighter-weight
alternative to Babel for projects that don't need everything Babel provides.

First install `flow-remove-types` with either
[Yarn](https://yarnpkg.com/) or [npm](https://www.npmjs.com/).

```sh
{{include.install_command}} flow-remove-types
```

If you then put all your source files in a `src` directory you can compile them
to another directory by running:

```sh
{{include.run_command}}
```

You can add this to your `package.json` scripts easily.

```json
{
  "name": "my-project",
  "main": "lib/index.js",
  "scripts": {
    "build": "flow-remove-types src/ -d lib/",
    "prepublish": "{{include.package_manager}} run build"
  }
}
```

> **Note:** You'll probably want to add a `prepublish` script that runs this
> transform as well, so that it runs before you publish your code to the npm
> registry.
