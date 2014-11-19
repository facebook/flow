/* @flow */

function length(x) {
  if (x) {
    return x.length;
  } else {
    return 0;
  }
}

var total = length("Hello") + length(null);
