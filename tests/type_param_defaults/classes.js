/* @flow */

class A<T> {
  p: T;
  constructor(p: T) {
    this.p = p;
  }
}

// Test out simple defaults
class B<T = string> extends A<T> {}

var b_number: B<number> = new B(123);
var b_void: B<void> = new B();
var b_default: B<> = new B('hello');
var b_any: B = new B(10);

(b_number.p: boolean); // Error number ~> boolean
(b_void.p: boolean); // Error void ~> boolean
(b_default.p: boolean); // Error string ~> boolean
(b_any.p: boolean); // This is fine due to any

class C<T: ?string = string> extends A<T> {}

var c_number: C<number> = new C(123); // Error number ~> ?string
var c_void: C<void> = new C();
var c_default: C<> = new C('hello');
var c_any: C = new C('hello');

(c_void.p: boolean); // Error void ~> boolean
(c_default.p: boolean); // Error string ~> boolean
(c_any.p: boolean); // This is fine due to any

class D<S, T = string> extends A<T> {}
var d_number: D<mixed, number> = new D(123);
var d_void: D<mixed, void> = new D();
var d_default: D<mixed> = new D('hello');
var d_too_few_args: D<> = new D('hello'); // Error too few tparams
var d_too_many: D<mixed, string, string> = new D('hello'); // Error too many tparams
var d_any: D = new D(10);

(d_number.p: boolean); // Error number ~> boolean
(d_void.p: boolean); // Error void ~> boolean
(d_default.p: boolean); // Error string ~> boolean
(d_any.p: boolean); // This is fine due to any

class E<S: string, T: number = S> {} // Error: string ~> number
class F<S: string, T: S = number> {} // Error: number ~> string

class G<S: string, T = S> extends A<T> {}

var g_default: G<string> = new G('hello');

(g_default.p: boolean); // Error string ~> boolean
