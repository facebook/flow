**Add a `devDependency` on the `flow-bin` npm package:**

```sh
npm install --save-dev flow-bin
```

**Add a `"flow"` script to your `package.json`:**

```json
{
  "name": "my-flow-project",
  "version": "1.0.0",
  "devDependencies": {
    "flow-bin": "^0.118.0"
  },
  "scripts": {
    "flow": "flow"
  }
}
```

**Run Flow:**

The first time, run:

```sh
npm run flow init
```

```
> my-flow-project@1.0.0 flow /Users/Projects/my-flow-project
> flow "init"
```

After running `flow` with `init` the first time, run:

```sh
npm run flow
```

```
> my-flow-project@1.0.0 flow /Users/Projects/my-flow-project
> flow

No errors!
```
