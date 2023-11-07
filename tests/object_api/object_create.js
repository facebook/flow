/* @flow */

class C {
  foo: string;
}

// OK, `instanceof C` would be true
Object.create(C.prototype) as C;

// OK, `instanceof C` would be true
Object.create(new C()) as C;

// error, object literals don't structurally match instances
({foo: 'foo'}) as C;

// error, object types don't structurally match instances
type O = {foo: string};
declare var o: O;
o as C;

Object.create(({}: Object)) as C; // OK: AnyT might be C, who knows
