import * as React from 'react';

declare component Base();

declare component Wrapper(label: string | renders Base);

component UseWrapper(
  label: string | renders Base
) {
  return <Wrapper label={label} />; // ok, no more spurious error
}
