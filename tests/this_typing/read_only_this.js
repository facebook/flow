// @flow

class X<+T> {
    // `this` is inherently covariant, so we allow
    // T to appear in an input position to the
    // `this` parameter alone
    foo(this : X<T>) {}
}
