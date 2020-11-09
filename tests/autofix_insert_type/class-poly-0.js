// @flow

class D<T> {}
class C<T> extends D<T> {
  m(x: T) {
    //$FlowFixMe[escaped-generic]
    return x;
  }
}

module.exports = C;
