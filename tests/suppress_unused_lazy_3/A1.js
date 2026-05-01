// @flow

export function foo(x: string): number {
    return 1;
}

// $FlowFixMe[incompatible-type] - used suppression in A1.js
0 as 1;
