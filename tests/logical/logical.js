/* @flow */

/**
 * A falsy variable on the left side of &&
 */
function logical1a(): number {
  // expected `: boolean`
  var x = false as const;
  return x && '123';
}

/**
 * A truthy variable on the left side of &&
 */
function logical1b(): string {
  var x = true as const;
  return x && '123';
}

/**
 * A literal on the left side of &&
 */
function logical2a(): number {
  // expected `: boolean`
  return false && '123';
}

/**
 * A literal on the left side of &&
 */
function logical2b(): number {
  return 0 && '123';
}

/**
 * A literal on the left side of &&
 */
function logical2c(): string {
  return '' && 123;
}

/**
 * A literal on the left side of &&
 */
function logical2d(): string {
  return true && '123';
}

/**
 * A literal on the left side of &&
 */
function logical2e(): number {
  return 'foo' && 123;
}

/**
 * A literal on the left side of &&
 */
function logical2f(): string {
  return 123 && 'foo';
}

/**
 * A literal on the left side of &&
 */
function logical2g(): string {
  return [1, 2, 3] && 'foo';
}

/**
 * A literal on the left side of &&
 */
function logical2h(x: {a: number}): string {
  return x && 'foo';
}

/**
 * A literal on the left side of &&
 */
function logical2i(x: Object): string {
  return x && 'foo';
}

/**
 * A literal on the left side of &&
 */
function logical2j(x: (a: number) => number): string {
  return x && 'foo';
}

/**
 * A literal on the left side of &&
 */
function logical2k(x: Function): string {
  return x && 'foo';
}

/**
 * A literal on the left side of &&
 */
function logical2l(x: {}): string {
  return x && 'foo';
}

/**
 * A literal on the left side of &&
 */
function logical2m(x: {||}): string {
  return x && 'foo';
}

/**
 * A literal on the left side of &&
 * We allow `$Exact<void>`
 */
function logical2n(x: $Exact<void>): void | string {
  return x && 'foo';
}

/**
 * An expression on the left side of &&
 */
function logical3a(): string {
  // expected `: boolean`
  var x: ?number = null;
  return x != null && x > 10;
}

/**
 * An expression on the left side of &&
 */
function logical3b(): number {
  // expected `: boolean | number`
  var x: ?number = null;
  return x != null && x;
}

/**
 * An expression on the left side of &&
 */
function logical3c(): ?number {
  // expected `: boolean | ?number`
  var x: ?number = null;
  return x != undefined && x;
}

/**
 * Usage of `$Exact` on non-literal.
 * Could be falsey or not.
 */
function logical3d<T: void | {||}>(x: $Exact<T>): $Exact<T> | string {
  return x && 'foo';
}

/**
 * Maybe truthy returns both types
 */
function logical4(x: boolean): string {
  // expected `: boolean | string`
  return x && '123';
}

/**
 * A falsy variable on the left side of ||
 */
function logical5a(): number {
  var x = false as const;
  return x || 0;
}

/**
 * A maybe variable on the left side of ||
 */
function logical5b(): number {
  var x: ?number = null;
  return x || 0;
}

/**
 * A truthy variable on the left side of ||
 */
function logical5c(): string {
  // expected `: boolean`
  var x = true as const;
  return x || 0;
}

/**
 * A literal on the left side of ||
 */
function logical6a(): string {
  return false || '123';
}

/**
 * A literal on the left side of ||
 */
function logical6b(): string {
  return 0 || '123';
}

/**
 * A literal on the left side of ||
 */
function logical6c(): number {
  return '' || 123;
}

/**
 * A literal on the left side of ||
 */
function logical6d(): number {
  // expected `: boolean`
  return true || '123';
}

/**
 * A literal on the left side of ||
 */
function logical6e(): string {
  return 'foo' || 123;
}

/**
 * A literal on the left side of ||
 */
function logical6f(): number {
  return 123 || 'foo';
}

/**
 * A literal on the left side of ||
 */
function logical6g(x: {}): {} {
  return x || 'foo';
}

/**
 * A literal on the left side of ||
 */
function logical6h(x: {||}): {||} {
  return x || 'foo';
}

/**
 * A literal on the left side of ||
 * We allow `$Exact<void>`
 */
function logical6i(x: $Exact<void>): string {
  return x || 'foo';
}

/**
 * A composite && and ||
 */
function logical7a(): number {
  var x: ?number = null;
  return (x != null && x) || 0;
}

/**
 * A composite && and || where the truthiness is unknown
 */
function logical7b(x: boolean, y: number): number {
  return (x && y) || 0;
}

/**
 * A composite && and ||
 */
function logical7c(x: string): number {
  return (x && 1) || 0;
}

/**
 * A composite && and ||
 */
function logical7d(x: number): string {
  return (x && 'foo') || 'bar';
}

/**
 * A composite && and ||
 */
function logical7e(x: number): string {
  return (false && x) || 'bar';
}

/**
 * A composite || and &&
 *
 * `x || 0` always returns a number (never a boolean) and then short
 * circuits the && because 0 is falsy, so this should just return number.
 */
function logical8a(): number {
  var x = false as const;
  return (x || 0) && 'foo';
}

/**
 * A composite || and &&
 *
 * `x || 1` always returns something truthy, so this returns a string
 */
function logical8b(): string {
  var x = false;
  return (x || 1) && 'foo';
}

/**
 * A composite || and &&
 *
 * `x` is always truthy, so this returns a string
 */
function logical8c(): string {
  var x = true;
  return (x || 1) && 'foo';
}

/**
 * A composite || and &&
 */
function logical8d(): number {
  var x = false as const;
  return x || (0 && 'foo');
}

/**
 * A composite || and &&
 */
function logical8e(): string {
  var x = false as const;
  return x || (1 && 'foo');
}

/**
 * A composite || and &&
 */
function logical8f(): string {
  // expected `: boolean`
  var x = true;
  return x || (1 && 'foo');
}

/**
 * A composite || and ||
 */
function logical9a(x: number, y: string): number | string {
  // expected `: number | string | boolean`
  return x || y || false;
}

/**
 * A composite || and ||
 */
function logical9b(x: number, y: string): number | string {
  return false || x || y;
}

/**
 * A composite || and ||
 */
function logical9c(x: number, y: boolean): string {
  return 'a' || x || y;
}

/**
 * A composite && and &&
 */
function logical10a(x: number, y: string): number | string {
  // expected `: number | string | boolean`
  return x && y && false;
}

/**
 * A composite && and &&
 */
function logical10b(x: number, y: string): Array<any> {
  // expected `: boolean`
  return false && x && y;
}

/**
 * A composite && and &&
 */
function logical10c(x: number, y: string): Array<any> {
  // expected `number | boolean`
  return x && false && y;
}

/**
 * || in a loop
 */
function logical11a(): number {
  var y = 1;
  for (var x = 0; x < 5; x++) {
    y = y || true; // error `y` is a number
  }
  return y;
}

/**
 * || in a loop
 */
function logical11b(y: number): number {
  for (var x = 0; x < 5; x++) {
    y = y || true; // expected a number
  }
  return y;
}

/**
 * && in a loop
 */
function logical12a(): number {
  var y: number | boolean = 1;
  var z = true;
  for (var x = 0; x < 5; x++) {
    y = z && y;
    z = false;
  }
  return y;
}

/**
 * && in a loop
 */
function logical12b(y: number): number {
  for (var x = 0; x < 5; x++) {
    y = y && true; // expected a number
  }
  return y;
}

/**
 * Complex &&
 */
function logical13(x: number): Array<{x: string}> {
  return [
    {x: x && 'bar'},
    {x: true && 'bar'},
    {x: true && false},
    {x: false && false},
    {x: 1 && 'bar'},
    {x: 'foo' && 'bar'},
    {x: 'foo' && 'bar'},
    {x: 'foo' && 'bar'},
  ];
}

/**
 * Complex ||
 */
function logical14(x: number): Array<{x: string}> {
  return [
    {x: x || 'bar'},
    {x: false || 'bar'},
    {x: false || true},
    {x: true || false},
    {x: 0 || 'bar'},
    {x: 'foo' || 'bar'},
    {x: 'foo' || 'bar'},
    {x: 'foo' || 'bar'},
  ];
}

/**
 * || in an addition
 */
function logical15a(x: number): number {
  return 5 + (x || 7);
}

/**
 * || in an addition
 */
function logical15b(x: number): number {
  return (x || 7) + 5;
}

/**
 * && in an addition
 */
function logical15c(x: number): number {
  return 5 + (x && 7);
}

/**
 * && in an addition
 */
function logical15d(x: number): number {
  return (x && 7) + 5;
}

/**
 * || in a comparison
 */
function logical16a(x: number): boolean {
  return 5 < (x || 7);
}

/**
 * || in a comparison
 */
function logical16b(x: number): boolean {
  return (x || 7) < 5;
}

/**
 * && in a comparison
 */
function logical16c(x: number): boolean {
  return 5 < (x && 7);
}

/**
 * && in a comparison
 */
function logical16d(x: number): boolean {
  return (x && 7) < 5;
}

/**
 * || in an equality
 */
function logical17a(x: number): boolean {
  return 5 == (x || 7);
}

/**
 * || in an equality
 */
function logical17b(x: number): boolean {
  return (x || 7) == 5;
}

/**
 * && in an equality
 */
function logical17c(x: number): boolean {
  return 5 == (x && 7);
}

/**
 * && in an equality
 */
function logical17d(x: number): boolean {
  return (x && 7) == 5;
}

/**
 * Expressions on each side that return truthy things
 */
function logical18a(x: number, y: number): number {
  return x - 1 || y - 1;
}

/**
 * Sentinel properties should not interfere
 */
function logical18b(x: {a: number}, y: {b: number}): number {
  return x.a - 1 || y.b - 1;
}

/**
 * Layer of indirection in the LHS (get prop)
 */
function logical19a(x: {y: string, z: boolean}): boolean {
  return x.y && x.z; // error: x.y is a string
}
function logical19b(x: {y: string, z: boolean}): boolean {
  return x.y || x.z; // error: x.y is a string
}

/**
 * Intersection on the LHS
 */
function logical20(x: {y: string} & {}): void {
  (x && x.y) as string; // ok
}
function logical21(x: {y: string} & {}): void {
  (x && x.y) as number; // error, x.y is a string (no error about x)
}

function logical22() {
  type indirection1 = ?number;
  type indirection2 = ?string;
  declare var a: indirection1 | indirection2;

  (a ?? false) as empty; // should error on `a` and `false`

  type indirection3 = 0;
  type indirection4 = '';
  declare var b: indirection3 | indirection4;

  (b && false) as empty; // should only error on `b`
}

/**
 * Intersection on the left side of ||
 */

declare class RecordInstance<X> {}
type RecordOf<Values: Object> = RecordInstance<Values> & Values;
type Rec = RecordOf<{f: number}>;

function logical7f(a: ?Rec, b: Rec): Rec {
  return a || b; // okay
}

function logicalWithTypeApp() {
  type Nullable<T> = T | null;
  declare const nullable: Nullable<string>;
  const nullableStringOrNumber = nullable ?? 0;
  nullableStringOrNumber as string; // error: number ~> string
}

function not_truhty() {
  declare var n: number;
  declare var s: string;
  declare var b: boolean;
  let n_ = n && {};
  let s_ = s && {};
  let b_ = b && {};
  n_ as 0 | {}; // okay
  s_ as "" | {}; // okay
  b_ as false | {}; // okay
  n_ = 1; // error number ~> 1
  s_ = "1"; // error "1" ~> ""
  b_ = true; // error true ~> false
}
