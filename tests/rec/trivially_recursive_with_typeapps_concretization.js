type T1<X> = T1<X>
type T2<X> = ?T2<X>
type A<X> = B<X>
type B<X> = A<X>
type I<X> = I<X> & I<X>

function conditionTest(t1: T1<string>, t2: T2<number>, a: A<string>, i: I<string>) {
    if (t1) {} // ok
    if (t2) {} // only the null and void members are concretized
    if (a) {} // ok
    if (i) {} // ok
}

// A type application whose expansion was rolled back with a failed speculative branch
// is expanded again in the next branch.
type Bad<X> = X['missing'] | null;
type W<Z> = Z & Z;
declare var m: W<Bad<{}>>;
const r = match (m) { 1 => 1 }; // error: `null` is not checked
