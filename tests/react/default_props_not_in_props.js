// A default prop that the exact props type does not declare

import * as React from 'react';

class K extends React.Component<{}> {
  static defaultProps: {p: number} = {p: 1};
}

const config: React.ComponentProps<typeof K> = {}; // ERROR: the default prop is not declared in K's props
<K />; // ERROR: also reported at the element
