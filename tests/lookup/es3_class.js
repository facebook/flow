// Props read from fun-constructed objects must be compatible with any property
// writes to the prototype.
function A() {}
var a = new A;
(a.p: string); // installs shadow prop on A.prototype
A.prototype.p = 0; // error: incompatible write hits shadow prop

// Props set on fun-constructed objects must be compatible with any property
// reads from the prototype.
function B() {}
var b = new B;
b.p = 0; // installs shadow prop on B.prototype
B.prototype.m = function() {
  (this.p: string); // error: incompatible read hits shadow prop
}
b.m();

// Props read from fun-constructed objects needn't be compatible with each other
// if the prototype is never constrained.
function C() {}
var c1 = new C;
var c2 = new C;
(c1.p: string); (c2.p: number); // ok, each object has a separate `p`

// Props written to fun-constructed objects needn't be compatible with each
// other if the prototype is never constrained.
function D() {}
var d1 = new D;
var d2 = new D;
d1.p = 0; d2.p = ""; // ok, each object has a separate `p`
(d1.p: void); // error, number ~> void (*not* string ~> void)
(d2.p: void); // error, string ~> void (*not* number ~> void)

// Fun-constructed objects can be compatible with other object types, when the
// properties are compatible.
function E() {}
E.prototype.p = 0;
var e: {p: string} = new E; // error, number ~> string

function F() {}
var f: {p: string} = new F; // error, `p` not found on `F.prototype` (yet)
F.prototype.p = 0; // error: number ~> string (eventually)
