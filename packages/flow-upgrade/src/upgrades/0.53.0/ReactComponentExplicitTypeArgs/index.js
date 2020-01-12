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

exports.title =
  'Move inferred React.Component type arguments to their generic positions.';

exports.description = `
The recommended way to write React components used to be:

${Styled.codeblock(
  `
import React from 'react';

type DefaultProps = { /* ... */ };
type Props = { /* ... */ };
type State = { /* ... */ };

class MyComponent extends React.Component {
  static defaultProps: DefaultProps = { /* ... */ };

  props: Props;
  state: State = { /* ... */ };

  render() {
    return /* ... */;
  }
}`.slice(1),
)}

While you would write React.Component in this way without type arguments the
signature for React.Component was in fact:
React.Component<DefaultProps, Props, State>. So for Flow to get from the
component style above to a place where React components had the correct type
arguments it would turn:

${Styled.codeblock(
  `
class MyComponent extends React.Component {`.slice(1),
)}

...into:

${Styled.codeblock(
  `
class MyComponent extends React.Component<*, *, *> {`.slice(1),
)}

Where the star (*) meant "infer." However, this approach is difficult to
understand, reduces type trustworthiness, and has some negative impacts on
performance as Flow needs to carry inference information around everywhere.

This upgrade runs a codemod to make the type arguments you pass into
React.Component explicit. We take the code in the first example above and turn
it into:

${Styled.codeblock(
  `
import React from 'react';

type DefaultProps = { /* ... */ };
type Props = { /* ... */ };
type State = { /* ... */ };

class MyComponent extends React.Component<DefaultProps, Props, State> {
  static defaultProps = { /* ... */ };

  state = { /* ... */ };

  render() {
    return /* ... */;
  }
}`.slice(1),
)}`.slice(1);

exports.transformPath = path.join(__dirname, './codemod.js');
