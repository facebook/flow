interface A {
  x: number;
  x(): void;
}

interface B {
  x(): void;
  x: number;
}

declare const a: A;
a.x as empty; // error: function ~> empty

declare const b: B;
b.x as empty; // error: number ~> empty
