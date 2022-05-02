// @flow

import * as React from 'react';

declare var d: ?{foo: {bar: () => React.MixedElement}};
if (d != null) {
  <d.foo.bar />;
}
