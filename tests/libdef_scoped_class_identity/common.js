import type {ScopedFoo} from './foo/foo'

export type CommonFoo = Foo;

declare const foo: ScopedFoo;
foo as CommonFoo;
