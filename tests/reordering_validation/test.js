//@flow

function invalidate_y() {
  y = 42;
}

var y = null;
invalidate_y();
(typeof y !== 'number' && (y: empty)); // Errors because we don't actually reorder
y = 42;
(y: number);
