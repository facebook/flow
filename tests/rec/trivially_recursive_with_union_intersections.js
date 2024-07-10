type T1 = ?T1; // error: cyclic
type T2 = string | T2; // error: cyclic
type T3 = T3 & string; // error: cyclic

function reposTest(t1: T1, t2: T2, t3: T3) {
    t1; // ok
    t2; // ok
    t3; // ok
}
