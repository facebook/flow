// Regression test: destructured parameter binding must not shadow the type
// annotation. The type annotation `: {Foo: Foo}` should resolve `Foo` against
// the outer `import typeof` binding, not the freshly-introduced destructured
// value `Foo`. Otherwise the type signature builder reports a spurious
// trivially-recursive-definition error.
import typeof Foo from './Foo';

declare var FooClass: Foo;

export const useFoo = ({Foo}: {Foo: Foo}): void => {};
useFoo({Foo: FooClass});

// Same pattern with array destructuring.
export const useFooArray = ([Foo]: [Foo]): void => {};
useFooArray([FooClass]);
