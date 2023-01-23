class D<X> { }
class B<X> extends D<X> { }
class C<X> extends B<X> { }
((new C: C<number>): D<string>) // error: number ~/~ string
