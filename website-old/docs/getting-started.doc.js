/* @flow */
/*
---
id: getting-started
title: Getting started with Flow
permalink: /docs/getting-started.html
next: five-simple-examples.html
---
*/

/*
  To demonstrate how to add Flow to a project, let's set up an example npm project.

  First, we'll make an npm package for our example project called "get_started":

  ```bash
  $> mkdir -p get_started
  $> cd get_started
  $> echo '{"name": "get_started", "scripts": {"flow": "flow; test $? -eq 0 -o $? -eq 2"}}' > package.json
  ```

  Next we'll add Flow to our project:

  ```bash
  $> touch .flowconfig
  $> npm install --save-dev flow-bin
  ```

  And now we can start writing some code!

  **index.js**
*/

// @flow

// $DocIssue
var str = 'hello world!';
console.log(str);

/*
  Note that we've added `// @flow` to the top of our file. This indicates to Flow
  that we want this file to be checked. **If we don't add this flag to the top of
  the file, Flow will assume that the file isn't ready to be checked yet and
  Flow will not attempt to type check the file.**

  Ok, now let's run Flow and see what it has to say about the code we just wrote:

  ```bash
  $> npm run-script flow

  > test@ flow /get_started
  > flow

  No errors!
  ```

  Woohoo! No errors! Ok, now let's add a trivial type error just to see what
  happens:

  **index.js**
*/

// $NoCliOutput
// @flow

// $ExpectError
var str: number = 'hello world!';
console.log(str);

/*
  ```bash
  $> npm run-script flow

  > test@ flow /get_started
  > flow 2> /dev/null

  index.js:3
    3: var str: number = 'hello world!';
                         ^^^^^^^^^^^^^^ string. This type is incompatible with
    3: var str: number = 'hello world!';
                ^^^^^^ number


  Found 1 error
  ```

  Cool -- Flow noticed that we're assigning a string to a number and gave an
  error.

  Before we fix this type error, let's add Babel to our project so we can
  try running our code:

  ```bash
  $> npm install -g babel-cli
  $> npm install --save-dev babel-plugin-transform-flow-strip-types
  $> echo '{"plugins": ["transform-flow-strip-types"]}' > .babelrc
  ```

  Now we can try running our code *in spite* of the type error and see what
  happens:

  ```bash
  $> babel-node index.js
  hello world!
  ```

  It works! As you can see Flow does not prevent you from running the code you
  have written even if there are type errors. It is considered best practice to
  never publish your project when it has type errors, but often at development
  time it's useful to try running code even before it fully typechecks (usually
  for debugging or ad-hoc testing). This is one of the benefits of gradual typing
  and Flow is designed to support this and stay out of your way as much as you
  need it to during development.

  Note that we ran our code using `babel-node` rather than just vanilla `node` to
  run index.js. [`babel-node`](https://babeljs.io/docs/usage/cli/#babel-node)
  comes with `babel-cli` and is just a thin wrapper around vanilla `node` that
  first intercepts and transpiles JS code before running it with `node`.

  Check out the [Running Flow Code](/docs/running.html) section for more details
  on how we recommend compiling and publishing Flow code to npm and into
  production.

  ### Next steps

  Now that we know how to set Flow up, let's take a quick look at
  [a few examples](/docs/five-simple-examples.html) that are
  [included in the Flow repo](https://github.com/facebook/flow/tree/master/examples):

  1. [Hello Flow!](/docs/five-simple-examples.html#hello-flow)
  2. [Adding type annotations](/docs/five-simple-examples.html#adding-type-annotations)
  3. [Nullable types](/docs/five-simple-examples.html#nullable-types)
  4. [Arrays](/docs/five-simple-examples.html#arrays)
  5. [Dynamic code](/docs/five-simple-examples.html#dynamic-code)
*/
