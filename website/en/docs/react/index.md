---
layout: guide
---

Developers will often use Flow and React together, so it is important that Flow
can effectively type both common and advanced React patterns. This guide will
teach you how to use Flow to create safer React applications.

In this guide we will assume you know [the React basics][] and focus on adding
types for patterns you are already familiar with. We will be using examples
based on `react-dom`, but all of these patterns work in other environments
like `react-native` as well.

[the React basics]: https://facebook.github.io/react/docs/hello-world.html

## Setup Flow with React <a class="toc" id="toc-setup-flow-with-react" href="#toc-setup-flow-with-react"></a>

Flow and Babel work well together, so it doesn't take much to adopt Flow as a
React user who already uses Babel. If you need to setup Babel with Flow, you can
follow [this guide](../tools/babel/).

Babel also
[works out of the box with Create React App](../tools/create-react-app/),
just install Flow and create a `.flowconfig`.
