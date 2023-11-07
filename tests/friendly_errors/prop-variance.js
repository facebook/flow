/**
 * @format
 * @flow
 */

declare var any: any;
declare opaque type T;

any as {p: T} as {p: T}; // Ok
any as {p: T} as {+p: T}; // Ok
any as {p: T} as {-p: T}; // Ok
any as {+p: T} as {p: T}; // Error: read-only ~> writable
any as {+p: T} as {+p: T}; // Ok
any as {+p: T} as {-p: T}; // Error: read-only ~> write-only
any as {-p: T} as {p: T}; // Error: write-only ~> readable
any as {-p: T} as {+p: T}; // Error: write-only ~> read-only
any as {-p: T} as {-p: T}; // Ok
