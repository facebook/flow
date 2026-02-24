import * as React from 'react';

async component AsyncComp() { // ERROR: async component syntax not enabled
  await Promise.resolve();
  return <div />;
}

async hook useAsyncHook(): string { // ERROR: async hook syntax not enabled
  const result = await Promise.resolve("hello");
  return result;
}
