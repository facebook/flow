class A<X, Y, Z> {}
class B extends A<string, number, boolean> {}
class C<X, Y, Z> extends B {}

var c: C<number, string, Array<boolean>> = new C; // none of the type args matter
var a: A<string, number, Array<boolean>> = c; // the third type arg is incorrect
