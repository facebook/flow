declare class Single {
  x: number;
  constructor(x: number): Single;
}
declare class Point extends Single {
  y: number;
  constructor(x: number, y: number): Point;
}
declare class ThreeD extends Point {
  z: number;
  constructor(x: number, y: number, z: number): ThreeD;
}

// Basic
{
  declare const x: Point;

  match (x) { // OK
    Point {const x, const y, ...} => {
      x as number;
      y as number;
    }
  }

  match (x) {
    Point {const x, const y} => { // ERROR: instances are inexact
      x as number;
      y as number;
    }
  }
}

// Obj matches instance
{
  declare const x: Point;

  match (x) { // OK
    {const x, const y, ...} => {
      x as number;
      y as number;
    }
  }
}

// Mixed obj and instance
{
  declare const x: Point | {x: string, y: boolean};

  match (x) { // OK
    Point {const x, const y, ...} => {
      x as number;
      y as number;
    }
    {const x, const y} => {
      x as string;
      y as boolean;
    }
  }

  match (x) { // OK (matches both instance and object)
    {const x, const y, ...} => {
      x as number | string;
      y as number | boolean;
    }
  }
}

// Super matches sub
{
  declare const x: Point;

  match (x) { // OK
    Single {const x, ...} => {
      x as number;
    }
  }
}
{
  declare const x: ThreeD;

  match (x) { // OK
    Single {const x, ...} => {
      x as number;
    }
  }
}

// Sub of value type: reachable, but doesn't contribute to exhaustiveness.
// Flow doesn't track all subclasses of a class, so a subclass pattern can't
// make a match exhaustive, but the runtime value could be that subclass, so
// the pattern is reachable and must not be reported as unused.
{
  declare const x: Point;

  match (x) { // ERROR: non-exhaustive (need `Point {...}`)
    ThreeD {...} => {} // OK: reachable (a `Point` could be a `ThreeD` at runtime)
  }
}

// Invalid constructor
{
  declare const x: Point;
  declare const S: string;

  match (x) { // ERROR: non-exhaustive
    S {...} => {} // ERROR
  }
}
{
  declare const x: Point;
  interface I {
    x: number;
    y: number;
  }

  match (x) { // ERROR: non-exhaustive
    I {...} => {} // ERROR
  }
}

// Anonymous class
{
  const C = class {
    a: string;
    b: boolean;
  };
  declare const x: C;

  match (x) { // OK
    C {const a, const b, ...} => {
      a as string;
      b as boolean;
    }
  }

  match (x) {} // ERROR
}

// Recursive extends doesn't cause infinite loop
{
  declare class C extends C {}
  declare const x: C;

  match (x) {
    C {...} => {} // Terminates
  }
}
{
  declare class B extends A {}
  declare class A extends C {}
  declare class C extends B {}
  declare const x: C;

  match (x) {
    B {...} => {} // Terminates
  }
}

// Duplicate property
{
  declare const x: Point;

  match (x) {
    Point {x: _, x: _, ...} => {} // ERROR
  }
}

// Invalid property
{
  declare const x: Point;

  match (x) {
    Point {1.3: _, ...} => {} // ERROR
    _ => {}
  }
}

// Invalid shorthand
{
  declare const x: Point;

  match (x) {
    Point {x, ...} => {} // ERROR
  }
}

// Subclass instance pattern: reachable but does not contribute to exhaustiveness
{
  declare class Animal {
    name: string;
  }
  declare class Cat extends Animal {
    breed: string;
  }

  declare const a: Animal;

  // The subclass pattern is reachable and the base pattern makes the match
  // exhaustive. The object part of the subclass pattern is validated, so the
  // `breed` binding is available.
  match (a) { // OK
    Cat {const breed, ...} => { // OK: not unused
      breed as string;
    }
    Animal {const name, ...} => {
      name as string;
    }
  }

  // A subclass pattern alone is not exhaustive: Flow does not track subclasses,
  // so the base class is still uncovered.
  match (a) { // ERROR: non-exhaustive (need `Animal {...}`)
    Cat {const breed, ...} => {}
  }

  // Once the base class is matched, the subclass pattern is genuinely unused:
  // every `Cat` is also an `Animal`, so the first pattern already covers it.
  match (a) {
    Animal {const name, ...} => {}
    Cat {const breed, ...} => {} // ERROR: unused
  }

  // The object part of a subclass pattern is still validated: instances are
  // inexact, so a subclass pattern without `...` is a non-exhaustive object
  // pattern.
  match (a) {
    Cat {const breed} => {} // ERROR: instances are inexact
    Animal {const name, ...} => {}
  }

  // The object part is validated against the subclass type: matching `breed`
  // (a `string`) against `null` can never match, so both the `null` pattern and
  // the rest are unused.
  match (a) {
    Cat {breed: null, ...} => {} // ERROR: `breed` is `string`, cannot be `null`
    Animal {const name, ...} => {}
  }
}

// Overlapping subclass patterns: an earlier subclass pattern that fully covers
// its class makes a later equal-or-narrower subclass pattern unused.
{
  declare class Animal {
    name: string;
  }
  declare class Cat extends Animal {
    breed: string;
  }
  declare class Siamese extends Cat {
    color: string;
  }

  declare const a: Animal;

  // Duplicate subclass pattern: the second is unused.
  match (a) {
    Cat {...} => {}
    Cat {...} => {} // ERROR: unused
    Animal {...} => {}
  }

  // Narrower subclass after a broader one: every `Siamese` is already covered by
  // the `Cat` pattern.
  match (a) {
    Cat {...} => {}
    Siamese {...} => {} // ERROR: unused
    Animal {...} => {}
  }

  // Broader subclass after a narrower one: the `Cat` pattern still matches the
  // non-`Siamese` cats, so it stays reachable.
  match (a) { // OK
    Siamese {...} => {}
    Cat {...} => {}
    Animal {...} => {}
  }

  // A non-total earlier subclass pattern does not cover its class, so a later
  // total subclass pattern for the same class stays reachable.
  match (a) {
    Cat {breed: null, ...} => {} // ERROR: `breed` is `string`, cannot be `null`
    Cat {...} => {} // OK: reachable (the previous pattern matches no `Cat`)
    Animal {...} => {}
  }
}

// Subclass narrows an inherited property: the binding in the subclass pattern
// gets the narrower subclass type. The property must be `readonly` (covariant)
// for the subclass to narrow it, and the base type is wider.
{
  declare class Animal {
    readonly size: string | number;
  }
  declare class Cat extends Animal {
    readonly size: string;
  }

  declare const a: Animal;

  match (a) { // OK
    Cat {const size, ...} => {
      size as string; // narrowed to `Cat`'s `size`
    }
    Animal {const size, ...} => {
      size as string | number;
    }
  }
}
