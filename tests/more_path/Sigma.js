
/* @providesModule Sigma */

class A { }

class B extends A { }

class C extends B { }

function bar(x:B) {
  if (x instanceof A) { x(); } // error
  else { x(); } // no error, since unreachable (x: B implies x: A)
}

function foo(x:A) {
  if (x instanceof C) { x(); } // error
  else { x() } // error
}

module.exports = "sigma";
