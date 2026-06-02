/**
 * @format
 * @flow
 */

declare const any: any;
declare opaque type T;

any as {p: T, ...} as {p: T, ...}; // Ok
any as {p: T, ...} as {readonly p: T, ...}; // Ok
any as {p: T, ...} as {writeonly p: T, ...}; // Ok
any as {readonly p: T, ...} as {p: T, ...}; // Error: read-only ~> writable
any as {readonly p: T, ...} as {readonly p: T, ...}; // Ok
any as {readonly p: T, ...} as {writeonly p: T, ...}; // Error: read-only ~> write-only
any as {writeonly p: T, ...} as {p: T, ...}; // Error: write-only ~> readable
any as {writeonly p: T, ...} as {readonly p: T, ...}; // Error: write-only ~> read-only
any as {writeonly p: T, ...} as {writeonly p: T, ...}; // Ok
