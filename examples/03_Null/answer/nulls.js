/* @flow */

function length(x) {
  if (x != null) {
    return x.length;
  } else {
    return 0;
  }
}

var total = length("Hello") + length(null);
