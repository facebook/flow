//@flow

function f(): number {
  return x;
}
var x = 42;


///

function invalidate_y() {
  y = 42;
}

var y = null;
invalidate_y();
(typeof y !== 'number' && (y: empty)); // Doesn't error because we've reordered this statement below `y = 42`
y = 42;
(y: number);



function k() {
  return h;
}
var h: number = k()
