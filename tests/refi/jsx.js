import * as React from 'react';

declare const d: ?{foo: {bar: () => React.MixedElement}};
if (d != null) {
  <d.foo.bar />;
}
