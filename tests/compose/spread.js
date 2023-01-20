// @flow

declare var compose: $Compose;
declare var fns: Array<(number) => number>;

(compose(...fns)(42): empty); // Error: unsupported
