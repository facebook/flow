declare var any: any;

type IdentityMap<O> = {[K in keyof O]: O[K]};
type Foo = IdentityMap<{ a: number, b: number }>;
declare var foo: Foo;

({
  a: 42, // OK
  b: 42, // OK
}: Foo);

({
  a: 'asd', // Error: string ~> number
  b: 'asd', // Error: string ~> number
}: Foo);

((any: {
  a: string, // Error: string ~> number
  b: string, // Error: string ~> number
}): Foo);

(({}: {}): Foo); // Error: `a` and `b` are not defined.

((any: {}): Foo); // Error: `a` and `b` are not defined.

(foo: {
  a: number, // OK
  b: number, // OK
});

(foo: {
  a: string, // Error: number ~> string
  b: string, // Error: number ~> string
});

(foo.a: empty); // Error: number ~> empty

({
  foo: 'asd', // OK
}: IdentityMap<{ foo: number | string }>);

({
  foo: 'asd', // OK
}: IdentityMap<{ foo: number } | { foo: string }>);

({
  foo: true, // Error: boolean ~> number | string
}: IdentityMap<{ foo: number | string }>);

({
  foo: true, // Error: boolean ~> number | string
}: IdentityMap<{ foo: number } | { foo: string }>);
