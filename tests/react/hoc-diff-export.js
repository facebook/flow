// @flow

import * as React from 'react';

type ClassComponent<TProps> =  Class<React.Component<TProps>>;

export function connect<TProps>(
  Component: React.ComponentType<TProps>,
  // React.ComponentType on return type don't work on multiple HOCs with $Diff
): ClassComponent<$Diff<TProps, { prop: number }>> {
  return (null: any);
}

export function connect2<TProps>(
  Component: React.ComponentType<TProps>,
): ClassComponent<$Diff<TProps, { prop2: number }>> {
  return (null: any);
}
