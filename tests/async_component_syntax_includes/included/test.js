// @flow
import * as React from 'react';

async component AsyncComp() { // OK: in included directory
  await Promise.resolve();
  return <div />;
}

async hook useAsyncHook(): Promise<void> { // OK: in included directory
  await Promise.resolve();
}
