interface V {
  virtualM(k: number): string;
  static virtualSM(k: number): string;
}

declare class X {
  x: number;
  static y: number;
  absM(this: this & V): string;
  static absSM(this: Class<this> & Class<V>): string;
}
X.y = 7;

var x = new X;
var s1: string = x.absM(); // NG: X doesn't implement V
var s2: string = X.absSM(); // NG: X doesn't implement V

declare class Y extends X {
  virtualM(k: number): string;
  static virtualSM(k: number): string;
}

var y = new Y;
var s3: string = y.absM();
var s4: string = Y.absSM();
