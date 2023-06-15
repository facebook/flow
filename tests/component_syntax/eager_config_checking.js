component Foo(foo: number, ...props: {foo: number}) {
  return null;
} 

component Poly<T>(foo: T, ...props: {foo: T}) { // No error on its own
  return null;
}
