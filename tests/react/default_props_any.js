// @flow

import React from 'react';

class Foo extends React.Component<Object, {required: number}> {
  static defaultProps: Object;
}

class Bar extends React.Component<any, {required: number}> {
  static defaultProps: any;
}

<Foo/>;
<Bar/>;
