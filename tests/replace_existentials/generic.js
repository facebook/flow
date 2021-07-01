// @flow

class C<X> { f: X }

class D extends C<*> {
    m() {
        this.f = "";
    }
}
