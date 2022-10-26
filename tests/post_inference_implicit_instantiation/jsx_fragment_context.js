//@flow

import * as React from 'react';

function PolyComponent<TItem>(props: {item?: TItem}) { return null }

// No error
const x = (
  <>
    <PolyComponent />
  </>
);
