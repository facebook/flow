// Props read from objects must be compatible with any property writes to the
// prototype.
var a_proto = {};
var a = Object.create(a_proto);
(a.p: string); // installs shadow prop on a_proto
a_proto.p = 0; // error: incompatible write hits shadow prop

// Props set on objects must be compatible with any property read from the
// prototype.
var b_proto = {};
var b = Object.create(b_proto);
b.p = 0; // installs shadow prop on b_proto
b.m = function() {
  (this.p: string); // error: incompatible read hits shadow prop
}
b.m();

// Props read from objects needn't be compatible with each other if the
// prototype is never constrained.
var c_proto = {};
var c1 = Object.create(c_proto);
var c2 = Object.create(c_proto);
(c1.p: string); (c2.p: number); // ok, each object has a separate `p`

// Props written to objects needn't be compatible with each other if the
// prototype is never constained.
var d_proto = {};
var d1 = Object.create(d_proto);
var d2 = Object.create(d_proto);
d1.p = 0; d2.p = ""; // ok, each object has a separate `p`
(d1.p: void); // error, number ~> void (*not* string ~> void)
(d2.p: void); // error, string ~> void (*not* number ~> void)

// Objects can be compatible with other object types, when the properties are
// compatible.
var e_proto = {};
e_proto.p = 0;
var e: {p: string} = Object.create(e_proto); // error, number ~> string

var f_proto = {};
var f: {p: string} = Object.create(f_proto); // error, `p` not found (yet)
f_proto.p = 0; // error: number ~> string (eventually)
