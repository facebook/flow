
/* @providesModule Arith */

function num(x:number) { }

function str(x:string) { }

function foo() {
  var x = 0;
  var y = "...";
  var z = {};
  num(x+x); // OK
  num(x+y);
  str(x+y); // OK
  str(x+x);
  str(z+y); // OK
  str(x+z);
}

module.exports = "arith";
