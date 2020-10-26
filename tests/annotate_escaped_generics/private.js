//@flow

class I<X> {
  #i;
  _f;
  _h(x: X) {
    return x;
  }
  g(x: X) {
    this.#i = x;
    this._f = x;
  }
}
