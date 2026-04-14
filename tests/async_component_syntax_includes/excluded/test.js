// @flow
import * as React from 'react';

async component AsyncComp() { // ERROR: not in included directory
  await Promise.resolve();
  return <div />;
}

async hook useAsyncHook(): Promise<void> { // ERROR: not in included directory
  await Promise.resolve();
}
