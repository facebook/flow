//@flow

var x = null,
  y = null;
function havoc() {
  x = 21;
  y = 31;
}
function havoc2() {
  y = 42; // a non provider write, so we dont get the special treatment for havocing that x does
}

havoc();

if (typeof x === 'number' && typeof y === 'number') {
  x as number;
  x as empty; // error, just to show that x is not empty
  y as number;
  havoc();
  x as number;
  x as empty; // error similarly
  y as number; // error b/c y was fully havoced
}

var z = null;

function havocz() {
  z = 42;
}

havocz();
z as number; // should fail, initial type of z was null

if (typeof z === 'number') {
  havocz();
  z as number; //ok
}

// Calls on require should also havoc.
if (typeof x === 'number' && typeof y === 'number') {
  x as number;
  x as empty; // error, just to show that x is not empty
  y as number;
  require('./empty');
  x as number;
  x as empty; // error similarly
  y as number; // error b/c y was fully havoced
}
