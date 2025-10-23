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

// Sub doesn't match super
{
  declare const x: Point;

  match (x) { // ERROR
    ThreeD {...} => {} // ERROR
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
