interface A {
  x: number;
  x(): void;
}

interface B {
  x(): void;
  x: number;
}

declare var a: A;
a.x as empty; // error: function ~> empty

declare var b: B;
b.x as empty; // error: number ~> empty
