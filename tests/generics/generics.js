class C<X> {
  x:X;
  constructor(x:X) { this.x = x; }
  get():X { return this.x; }
}

class D<T> {
  x:T;
  m<S>(z:S,u:T,v):S {
    this.x = u;
    v.u = u;
    return z;
  }
}

var d = new D();
var o = {};
var b = d.m(true,0,o);
var s:string = d.x;
var n:number = o.u;

class E<X> extends C<X> {
    //x:X;
    set(x:X):X { /*return x;*/ this.x = x; return /*this.x; */this.get(); }
}

var e = new E();
var x:string = e.set(0);

class F<X> { }
class G<X> extends F<Array<X>/* dummy token separator: pfff bug */> { }
class H<X> extends G<Array<X>/* dummy token separator: pfff bug */> {
    x:X;
    foo(x:X) { this.x = x; }
}

var h1 = new H();
h1.foo(["..."]);
var h2:F<Array<Array<Array<number>>>> = h1;
