// @flow

type Foo = {foo: number};

declare var x: Foo;
declare var mixed: mixed;
declare var any: any;
declare var empty: empty;
declare var maybe: ?Foo;
declare var union: Foo | null | void;

x?.foo as ?number; // no error, lint
mixed?.foo as ?number; // error, no lint
any?.foo as ?number; // no error, no lint
empty?.foo as ?number; // no error, no lint
maybe?.foo as ?number; // no error, no lint
union?.foo as ?number; // no error, no lint
