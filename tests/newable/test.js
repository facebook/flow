

class A {
  constructor(n: number) {}
  static sm(): void {}
  static datum: number;
}

class B extends A {
  constructor(s: string) {
    super(5);
  }
  static sm() {}
}

// Value B is newable, Type A non-newable => ClassT (NewableT B) ~> ClassT A //ok
var X1: Class<A> = B;

//Value B is newable, Type A newable => ClassT (NewableT B) ~> ClassT (NewableT A) //ng (incompatible ctor)
var X2: Class<Newable<A>> = B;

var a: A = new A(1); //ok
var b: A = new B("two"); //ok
var c: Newable<A> = new A(3); //ok
var d: Newable<A> = new B("four"); //ng //error gets blocked by 17
var e: B = new B("five"); //ok
var f: Newable<B> = new B("six"); //ok

var m: A = new a.constructor(10); //ng
var n: A = new c.constructor(11); //ok
var o: B = new e.constructor("twelve"); //ng
var p: B = new f.constructor("thirteen"); //ok

var t = new A(20); //ok
var u = new B("twenty one"); //ok
var v = new t.constructor(22); //ok
var w = new u.constructor("twenty three"); //ok

function fn(X: Class<Newable<A>>): Newable<A> {
  return new X(24); //ok
}
function gn(X: Class<B>): Newable<B> {
  return new X("twenty five"); //ng
}
function hn(X) {
  return new X(26);
}
var call1 = hn(A);
var call2 = hn(B);
