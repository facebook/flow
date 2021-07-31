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

exports.title = 'Simplify React.Component type arguments.';

exports.description = (`
A React.Component used to require three type arguments like this:
React.Component<DefaultProps, Props, State>. However, requiring DefaultProps
whenever using type arguments doesn't make much sense. Also, requiring State
for a component that does not use state, or in a consumer that doesn't care
about State also doesn't make much sense.

So we changed Flow so that we only require Props. If you write:
React.Component<Props> then State is assumed to be undefined and default props
will be inferred from the statics of your component class. A component written
without state but with default props in this new style looks like:

${Styled.codeblock(
  `
import React from 'react';

type Props = { /* ... */ };

class MyComponent extends React.Component<Props> {
  static defaultProps = { /* ... */ };
}`.slice(1),
)}

Default props is inferred from the static defaultProps object literal. If you
want a component with state add a second type argument:

${Styled.codeblock(
  `
import React from 'react';

type Props = { /* ... */ };
type State = { /* ... */ };

class MyComponent extends React.Component<Props, State> {
  static defaultProps = { /* ... */ };
}`.slice(1),
)}

This upgrade will remove DefaultProps from the type arguments of all your
React components.`.slice(1): string);

exports.transformPath = (path.join(__dirname, './codemod.js'): string);
