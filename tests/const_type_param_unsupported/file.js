function f1<const X>(): void {}
class C<const X> {
    m<const T>(x: X, t: T) {}
}
type T<const X> = void;
