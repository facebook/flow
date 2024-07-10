type T1<X> = T1<X> // no error. TODO(T110320325)
type T2<X> = ?T2<X> // no error. TODO(T110320325)

function reposTest(t1: T1<string>, t2: T2<number>) {
    t1; // ok
    t2; // ok
}
