// @flow

import * as React from 'react';

import { connect } from './hoc5-export.js';

type Props = {
  a: number,
  b: number,
  prop: number,
};

class MyComponent extends React.Component<Props> {}

// Needed to explicit type the component here otherwise
// MyEnhancedFunctionComponent would not type-check in the other file 
const MyFunctionComponent: React.ComponentType<Props> = (props) => { }

export const MyEnhancedComponent = connect(MyComponent);

export const MyEnhancedFunctionComponent = connect(MyFunctionComponent);
