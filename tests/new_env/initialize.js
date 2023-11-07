//@flow

var x;

function f() {
  x as number; //fine
}
x as number; // error
x = 10;

var y;
if (42 as any) {
  y = 10;
}

let g = function () {
  y as number; //fine
};

y as number; // error

if (42 as any) {
  var w;
} else {
  var w;
}

w as number; // Error, w is never initialized or assigned so cast to number is incorrect
