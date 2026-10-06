import * as React from 'react';

class K extends React.Component<{p: number}> {
  static set defaultProps(x: {p: number}) {}
}

<K />; // ERROR: `defaultProps` is write-only in the statics of `K`
