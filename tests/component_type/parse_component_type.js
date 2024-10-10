declare component Foo(bar: string) renders 'svg';
type FooTy = (component(bar: string) renders 'svg');

Foo as FooTy; // ok
