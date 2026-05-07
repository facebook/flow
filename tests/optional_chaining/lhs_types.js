// @flow

type Foo = {foo: number, ...};

declare const x: Foo;
declare const mixed: unknown;
declare const any: any;
declare const empty: empty;
declare const maybe: ?Foo;
declare const union: Foo | null | void;

x?.foo as ?number; // no error, lint
mixed?.foo as ?number; // error, no lint
any?.foo as ?number; // no error, no lint
empty?.foo as ?number; // no error, no lint
maybe?.foo as ?number; // no error, no lint
union?.foo as ?number; // no error, no lint
