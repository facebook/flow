class D<X> { }
class B<X> extends D<X> { }
class C<X> extends B<X> { }
new C as C<number> as D<string> // error: number ~/~ string
