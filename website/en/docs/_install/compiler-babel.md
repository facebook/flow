[Babel](http://babeljs.io/) is a compiler for JavaScript code that has
support for Flow. Babel will take your Flow code and strip out any type
annotations.

First install `@babel/core`, `@babel/cli`, and `@babel/preset-flow` with
either [Yarn](https://yarnpkg.com/) or [npm](https://www.npmjs.com/).

```sh
{{include.install_command}} @babel/core @babel/cli @babel/preset-flow
```

Next you need to create a `.babelrc` file at the root of your project with
`"@babel/preset-flow"` in your `"presets"`.

```json
{
  "presets": ["@babel/preset-flow"]
}
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
    "build": "babel src/ -d lib/",
    "prepublish": "{{include.package_manager}} run build"
  }
}
```

> **Note:** You'll probably want to add a `prepublish` script that runs this
> transform as well, so that it runs before you publish your code to the npm
> registry.
