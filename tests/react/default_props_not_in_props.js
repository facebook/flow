// A default prop that the exact props type does not declare. The error reports
// `p` as missing in the defaults and present in the props, the reverse of the
// actual situation; the order comes from the object kit's config merge.

import * as React from 'react';

class K extends React.Component<{}> {
  static defaultProps: {p: number} = {p: 1};
}

const config: React.ElementConfig<typeof K> = {}; // ERROR
