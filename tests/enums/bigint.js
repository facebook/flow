enum E1 of bigint {
    A = 1n
}

E1.A as E1; // OK
E1.A as bigint; // OK

enum E2 {
    A = 1n,
    B = 2n,
}

E2.A as E2; // OK
E2.A as bigint; // OK
