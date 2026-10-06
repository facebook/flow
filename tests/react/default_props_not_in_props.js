// A default prop that the exact props type does not declare

import * as React from 'react';

class K extends React.Component<{}> {
  static defaultProps: {p: number} = {p: 1};
}

const config: React.ElementConfig<typeof K> = {}; // ERROR: `p` is missing in the props
<K />; // ERROR: also reported at the element
