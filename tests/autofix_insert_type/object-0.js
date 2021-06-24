// @flow

var obj = {
  n(y) {
    return obj.m(y);
  },
  m(x) {
    return "";
  }
}

obj.n(0)
