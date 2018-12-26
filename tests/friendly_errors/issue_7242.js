// @flow

type Ob = {|+foo: {| +bar: string |}|};
declare var update: Ob;
({ foo: { bar: 'valid' }, ...update }: Ob); // Ok

type NestedOb = {|+foo: {| +bar: {| +foo: number |} |}|};
declare var update2: NestedOb;
({ foo: { bar: { foo: 123 } }, ...update2 }: NestedOb); // Ok
