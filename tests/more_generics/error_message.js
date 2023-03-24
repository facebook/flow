type D2<X,Y> = { p: Y };

class C2<X,Y> {
  x: { p: Y };
  foo(): D2<X,Y> { return this.x; }
}
