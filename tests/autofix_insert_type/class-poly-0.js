// @flow

class D<T> {}
class C<T> extends D<T> {
  m(x: T) {
    return x;
  }
}

module.exports = C;
