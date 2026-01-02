function test1() {
  declare function isAorB(x: 'a'|'b'|'c'): x is 'a' | 'b';

  declare var x: 'a'|'c';
  if (isAorB(x)) {
    x as 'a'; // okay
    x as 'b'; // error 'a' ~> 'b'
    x as 'c'; // error 'a' ~> 'c'
  }
}

function test2() {
  declare function isBorC(
    x: unknown
  ): implies x is Exclude<'a' | 'b' | 'c', 'a'>; // x is 'b' | 'c'

  declare const x: 'a' | 'c';
  if (isBorC(x)) {
    x as 'b'; // error 'c' ~> 'b'
    x as 'c'; // okay
  }
}

function test4() {
  type AllTypes = {
      A: null,
      B: null,
      C: null,
  };
  type AllClasses = KA | KB | KC;

  type KA = K<'A'>;
  type KB = K<'B'>;
  type KC = K<'C'>;

  class S<X: keyof AllTypes> {}
  class K<X: keyof AllTypes>  extends S<X> {}

  declare function isBorC(x: ?AllClasses): implies x is KB | KC;

  declare var x: AllClasses;
  if (isBorC(x)) {
    x as KB; // error KC ~> KB
    x as KB | KC; // okay
  }
}
