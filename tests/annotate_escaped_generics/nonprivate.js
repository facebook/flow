//@flow
//@preventMunge

class I<X> {
  _f;
  _h(x: X) {
    return x;
  }
  g(x: X) {
    this._f = x;
  }
}
