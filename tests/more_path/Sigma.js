
/* @providesModule Sigma */

class A { }

class B extends A { }

function bar(x:B) {
  if (x instanceof A) { x(); }
  else { x(); }
}

function foo(x:A) {
  if (x instanceof B) {
    bar(x);
  }
}

module.exports = "sigma";
