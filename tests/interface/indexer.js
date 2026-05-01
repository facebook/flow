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
  x.foo as boolean; // OK
  x['foo'] as boolean; // OK
  x['foo'] = true; // OK
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
  x as Invariant; // ERROR
  x as Covariant; // OK
  x as Contravariant; // OK
}
{
  declare const x: interface {[Keys]: number};
  x as Invariant; // ERROR
  x as Covariant; // OK
  x as Contravariant; // OK
}

{
  declare const x: {[Keys]: number | string};
  x as interface {[Keys]: number}; // ERROR
  x as interface {+[Keys]: number}; // ERROR
  x as interface {-[Keys]: number}; // OK
}
{
  declare const x: interface {[Keys]: number | string};
  x as interface {[Keys]: number}; // ERROR
  x as interface {+[Keys]: number}; // ERROR
  x as interface {-[Keys]: number}; // OK
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
  x['foo'] as string; // OK
  x[1] as boolean; // OK
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
  x['foo'] as string; // OK
  x['bar'] as boolean; // OK
  x['bar'] = true; // OK
}

interface Super {
  bar: string;
}
interface C extends Super {
  [string]: boolean;
}
{
  declare const x: C;
  x.bar as string; // OK
  x['bar'] as string; // OK
}
interface SuperWithMethod {
  bar: string;
  foo(): null;
}
interface D extends SuperWithMethod {
  [string]: () => number;
}
{
  declare const x: D;
  x.bar as string; // OK
  x['bar'] as string; // OK
  x['bar'] as empty; // ERROR
  x['xxx'] as () => number; // OK

  (x['bar'] = "foo"); // OK
  (x.bar = "foo"); // OK

  x.foo(); // OK
  x['foo'](); // OK
  x['foo'] as empty; // ERROR

  x['xxx']() as number; // OK
  x['xxx'] as empty; // ERROR
}

// Methods
interface M {
  foo(): number;
  [string]: boolean;
}
{
  declare const x: M;
  x.foo(); // OK
  x['foo'](); // OK
  x['xxx'] as boolean; // OK
}
interface M2 extends M {
  bar(): boolean;
}
{
  declare const x: M2;
  x.bar(); // OK
  x['bar'](); // OK
  x.foo(); // OK
  x['foo'](); // OK
}

// $ReadOnly type util
{
  declare const x: $ReadOnly<interface {[Keys]: number}>;
  declare const s: string;
  x['a']; // OK
  x['a'] = 1; // ERROR
  x[true]; // ERROR

  declare const y: {+a: number};
  y as $ReadOnly<interface {[string]: number}>; // OK
}
