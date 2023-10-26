// @flow

export function foo(x: string): number {
    return 1;
}

// $FlowFixMe[incompatible-cast] - used suppression in A1.js
(0: 1);
