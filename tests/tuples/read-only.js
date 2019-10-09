// @flow

declare var x: $ReadOnly<[number, string]>;

x[0] = 1; // can't write

const y: $ReadOnly<[number | string, string]> = x; // covariance

declare var z: $ReadOnly<$ReadOnly<[number]>>; // nested $ReadOnly works
