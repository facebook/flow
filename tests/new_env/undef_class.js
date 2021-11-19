//@flow

var x: empty = C; // should be a TDZ error, but for now just undefined
(x: C); // currently not actually a useful test, but when types are looked up in the new_env, should fail

class C {}
