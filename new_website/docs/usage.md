---
title: Usage
slug: /usage
---

Once you have [installed](../install/) Flow, you will want to get a feel of how to use Flow at the most basic level. For most new Flow projects, you will follow this general pattern:

- [Initialize your project](#toc-initialize-your-project) with `flow init`.
- Start the [Flow background process](#toc-run-the-flow-background-process) with `flow`.
- [Determine](#toc-prepare-your-code-for-flow) which files Flow will monitor with `// @flow`.
- [Write Flow code](#toc-write-flow-code) for your project.
- [Check your code](#toc-check-your-code) for type errors.

### Initialize Your Project {#toc-initialize-your-project}

Preparing a project for Flow requires only one command:

```sh
flow init
```

Run this command at the top level of your project to create one, empty file called [`.flowconfig`](../config/). At its most basic level, `.flowconfig` tells the Flow background process the root of where to begin checking Flow code for errors.

And that is it. Your project is now Flow-enabled.

> It is common to have an empty `.flowconfig` file for your project. However, you can [configure and customize Flow](../config/) in many ways through options available to be added to `.flowconfig`.

### Run the Flow Background Process {#toc-run-the-flow-background-process}

The core benefit to Flow is its ability to quickly check your code for errors. Once you have enabled your project for Flow, you can start the process that allows Flow to check your code incrementally and with great speed.

```sh
flow status
```

This command first starts a background process that will check all [Flow files](#toc-prepare-your-code-for-flow) for errors. The background process continues running, monitoring changes to your code and checking those changes incrementally for errors.

> You can also type `flow` to accomplish the same effect as `status` is the default flag to the `flow` binary.

> Only one background process will be running at any given time, so if you run `flow status` multiple times, it will use the same process.

> To stop the background process, run `flow stop`.

### Prepare Your Code for Flow {#toc-prepare-your-code-for-flow}

The Flow background process monitors all Flow files. However, how does it know which files are Flow files and, thus, should be checked? Placing the following **before any code** in a JavaScript file is the flag the process uses to answer that question.

```js
// @flow
```

This flag is in the form of a normal JavaScript comment annotated with `@flow`. The Flow background process gathers all the files with this flag and uses the type information available from all of these files to ensure consistency and error free programming.

> You can also use the form `/* @flow */` for the flag as well.

> For files in your project without this flag, the Flow background process skips and ignores the code (unless you call `flow check --all`, which is beyond the scope of basic usage).

### Write Flow Code {#toc-write-flow-code}

Now that all the setup and initialization is complete, you are ready to write actual Flow code. For each file that you have flagged with `// @flow`, you now have the full power of Flow and its type-checking available to you. Here is an example Flow file:

```js
// @flow

function foo(x: ?number): string {
  if (x) {
    return x;
  }
  return "default string";
}
```

Notice the types added to the parameter of the function along with a return type at the end of the function. You might be able to tell from looking at this code that there is an error in the return type since the function can also return an `int`. However, you do not need to visually inspect the code since the Flow background process will be able to catch this error for you when you [check your code](#toc-check-your-code).

### Check Your Code {#toc-check-your-code}

The great thing about Flow is that you can get near real-time feedback on the state of your code. At any point that you want to check for errors, just run:

```sh
# equivalent to `flow status`
flow
```

The first time this is run, the [Flow background process](#toc-run-flow-background-process) will be spawned and all of your Flow files will be checked. Then, as you continue to iterate on your project, the background process will continuously monitor your code such that when you run `flow` again, the updated result will be near instantaneous.

For the [code above](#toc-write-flow-code), running `flow` will yield:

```sh
test.js:5
  5:     return x;
                ^ number. This type is incompatible with the expected return type of
  3: function foo(x: ?number): string {
                               ^^^^^^ string
```
