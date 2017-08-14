// @flow

import * as React from 'react';

// Only `$Diff<P, { optional }>` and `{ optional} & TProps` approach worked on
// separated file
export function connect<TProps>(
  Component: React.ComponentType<TProps>,
): React.ComponentType<$Diff<TProps, { prop: number }>> {
  return (null: any);
}
