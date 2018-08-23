---
layout: guide
---

[Create React App](https://github.com/facebookincubator/create-react-app)
already supports Flow by default. All you need to do is
[install Flow](../../install/) and create a `.flowconfig` file by running
`flow init`.

```sh
create-react-app my-app && cd my-app
yarn add --dev flow-bin
yarn run flow init
```

Flow will be run as part of create-react-app's scripts.
