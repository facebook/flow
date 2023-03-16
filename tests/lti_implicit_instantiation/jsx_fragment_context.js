//@flow

import * as React from 'react';

function PolyComponent<TItem>(props: {item?: TItem}): null { return null }

// No error
const x = (
  <>
    <PolyComponent />
  </>
);
