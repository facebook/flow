// Tests that we correctly distribute mapped types over unions.
// A failure here would be trying to access a key from a branch
// that only exists in one branch of the union.
{
  type Homomorphic<O: {...}> = {[key in keyof O]: O[key]};
  declare const o: Homomorphic<{foo: number} | {bar: number}>; // OK
  (o: {foo: number} | {bar: number}); // OK
  (o: {foo: empty} | {bar: empty}); // ERROR x2
}

{
  type SemiHomomorphic<O: {...}, Keys: $Keys<O>> = {[key in Keys]: O[key]};
  declare const o: SemiHomomorphic<{foo: number, bar: number} | {foo: string, baz: number}, 'foo'>;
  (o: {foo: number} | {foo: string}); // OK
  (o: {}); // ERROR
}
