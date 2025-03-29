import type {CommonFoo} from '../common'

export type ScopedFoo = Foo;

declare const foo: CommonFoo;
foo as ScopedFoo;
