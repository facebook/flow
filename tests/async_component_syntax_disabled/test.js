import * as React from 'react';

async component AsyncComp() { // ERROR: async component syntax not enabled
  await Promise.resolve();
  return <div />;
}
