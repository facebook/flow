// Duplicate indexers
interface Ok {
  [key: string]: string;
}

interface Bad {
  [k1: string]: string;
  [k2: number]: number; // ERROR
}

// Access with string literal type
interface PropAndIndexer {
  [number]: string;
  foo: boolean;
}
{
  declare const x: PropAndIndexer;
  (x.foo: boolean); // OK
  (x['foo']: boolean); // Should be ok, but currently isn't
  x['foo'] = true; // Should be ok, but currently isn't
}

// Variance
type Keys = 'a' | 'b';

interface Invariant {
  [Keys]: number | string;
}
interface Covariant {
  +[Keys]: number | string;
}
interface Contravariant {
  -[Keys]: number;
}

{
  declare const x: Invariant;
  x['a']; // OK
  x['a'] = 1; // OK
  x['xxx']; // ERROR
}
{
  declare const x: Covariant;
  x['a']; // OK
  x['a'] = 1; // ERROR
  x['xxx']; // ERROR
}
{
  declare const x: Contravariant;
  x['a']; // ERROR
  x['a'] = 1; // OK
  x['xxx'] = 1; // ERROR
}
{
  declare const x: interface {+[string]: number};
  declare const s: string;
  x[s]; // OK
  x[s] = 1; // ERROR
  x[true]; // ERROR
}

{
  declare const x: {[Keys]: number};
  (x: Invariant); // ERROR
  (x: Covariant); // OK
  (x: Contravariant); // OK
}
{
  declare const x: interface {[Keys]: number};
  (x: Invariant); // Should error, but currently doesn't
  (x: Covariant); // OK
  (x: Contravariant); // OK
}

{
  declare const x: {[Keys]: number | string};
  (x: interface {[Keys]: number}); // ERROR
  (x: interface {+[Keys]: number}); // ERROR
  (x: interface {-[Keys]: number}); // OK
}
{
  declare const x: interface {[Keys]: number | string};
  (x: interface {[Keys]: number}); // Should error, but currently doesn't
  (x: interface {+[Keys]: number}); // Should error, but currently doesn't
  (x: interface {-[Keys]: number}); // OK
}

// Extends
interface NumIndexer {
  [number]: boolean;
}
interface A extends NumIndexer {
  foo: string;
}
{
  declare const x: A;
  (x['foo']: string); // Should be ok, but currently isn't
  (x[1]: boolean); // OK
  x[1] = true; // OK
}

interface StrIndexer {
  [string]: boolean;
}
interface B extends StrIndexer {
  foo: string;
}
{
  declare const x: B;
  (x['foo']: string); // Should be ok, but currently isn't
  (x['bar']: boolean); // OK
  x['bar'] = true; // OK
}

// Methods
interface M {
  foo(): number;
  [string]: boolean;
}
{
  declare const x: M;
  x.foo(); // OK
  x['foo'](); // Should be ok, but currently isn't
  (x['xxx']: boolean); // OK
}
interface M2 extends M {
  bar(): boolean;
}
{
  declare const x: M2;
  x.bar(); // OK
  x['bar'](); // Should be ok, but currently isn't
  x.foo(); // OK
  x['foo'](); // Should be ok, but currently isn't
}

