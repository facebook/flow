// @flow

import * as React from 'react';

import { connect, connect2 } from './hoc-diff-export.js';

type Props = {
  a: number,
  b: number,
  prop: number,
  prop2: number,
};

class MyComponent extends React.Component<Props> {}

const MyFunctionComponent: React.ComponentType<Props> = (props) => { }

export const MyEnhancedComponent = connect2(connect(MyComponent));

export const MyEnhancedFunctionComponent = connect2(connect(MyFunctionComponent));
