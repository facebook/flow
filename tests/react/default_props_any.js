// @flow

import React from 'react';

class Foo extends React.Component {
  static defaultProps: Object;
  props: {required: number};
}

class Bar extends React.Component {
  static defaultProps: any;
  props: {required: number};
}

<Foo/>;
<Bar/>;
