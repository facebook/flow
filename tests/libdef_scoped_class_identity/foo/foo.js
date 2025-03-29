import type {CommonFoo} from '../common'

export type ScopedFoo = Foo;

declare const foo: CommonFoo;
foo as ScopedFoo; // ok: both Foo should have the same identity
