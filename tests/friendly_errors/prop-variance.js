/**
 * @format
 * @flow
 */

declare var any: any;
declare opaque type T;

((any: {p: T}): {p: T}); // Ok
((any: {p: T}): {+p: T}); // Ok
((any: {p: T}): {-p: T}); // Ok
((any: {+p: T}): {p: T}); // Error: read-only ~> writable
((any: {+p: T}): {+p: T}); // Ok
((any: {+p: T}): {-p: T}); // Error: read-only ~> write-only
((any: {-p: T}): {p: T}); // Error: write-only ~> readable
((any: {-p: T}): {+p: T}); // Error: write-only ~> read-only
((any: {-p: T}): {-p: T}); // Ok

type Ob = {|+foo: {| +bar: string |}|};
declare var update: Ob;
({ foo: { bar: 'valid' }, ...update }: Ob); // Ok

type NestedOb = {|+foo: {| +bar: {| +foo: number |} |}|};
declare var update2: NestedOb;
({ foo: { bar: { foo: 123 } }, ...update2 }: NestedOb); // Ok
