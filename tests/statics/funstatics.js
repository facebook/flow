function C() { }
C.f = function() { return C.g(0); }
C.g = function(x) { return x; };
