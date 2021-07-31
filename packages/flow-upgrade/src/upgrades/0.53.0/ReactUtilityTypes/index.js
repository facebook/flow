/**
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *
 * @format
 * @flow
 */

const path = require('path');
const Styled = require('../../../Styled');

exports.kind = 'codemod';

exports.title = 'Remove and replace old React utility types.';

exports.description = (`
In the past Flow has some very confusing and inconvenient utility types for
React. We removed these utility types and replaced them with types which should
make a lot more sense. These types will make it a lot easier to effectively type
advanced React features like higher order components.

Some of the most powerful new utility types can be seen in the following
example:

${Styled.codeblock(
  `
import * as React from 'react';

function MyComponent(props: Props): React.Node {
  /* ... */
}
(React.createElement(MyComponent): React.Element<typeof MyComponent>);

class MyStatefulComponent extends React.Component<Props> {
  /* ... */
}
(React.createElement(MyComponent): React.Element<typeof MyStatefulComponent>);

function myHOC(
  Component: React.ComponentType<Props>,
): React.ComponentType<NewProps> {
  return class extends React.Component<NewProps> {
    /* ... */
  };
}`.slice(1),
)}

${Styled.list([
  `React.Node is the new return type for render methods. This codemod will
replace any render methods that return the type ?React.Element<any> with
React.Node.`,
  `React.Element<Component> now takes the type of a component as its type
argument instead of the type of a the component's props as it used to. This
codemod will upgrade everywhere you use React.Element<Props> to
React.Element<React.ComponentType<Props>>.`,
  `Speaking of React.ComponentType<Props>, this is a new utility type which
represents either a class component, or a stateless functional component. This
type is very useful when you want to type higher order components. We will
replace everywhere you use ReactClass<Props> in your codebase with
React.ComponentType<Props>. (ReactClass<Props> was very buggy and so this may
cause a lot of new errors!)`,
  `Also note that we used import * as React from 'react'. You must use the
import * syntax if you want to use the new utility types. This codemod will
update your code to use this new style of import.`,
])}`.slice(1): string);

exports.transformPath = (path.join(__dirname, './codemod.js'): string);
