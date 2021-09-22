//@flow

var x;

function f() {
  (x: number); //fine
}
(x: number); // error
x = 10;


var y;
if ((42: any)) {
  y = 10;
}

let g = function() {
  (y: number); //fine
};

(y: number); // error

if ((42: any)) {
  var w ;
} else {
  var w;
}

(w: number);


