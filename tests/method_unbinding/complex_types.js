interface I {
    m() : void
}

interface J {
    m() : void
}

declare const a : I & J;
declare const b : I | J;

a.m; // ok
b.m; // ok
let {m : ma} = a; // ok
let {m : mb} = b; // ok

interface K {
    m : () => void
}

declare const c : I & K;
declare const c2 : K & I;
declare const d : I | K;

c.m; // ok -- picking I.m
c2.m; // ok
d.m; // ok

let {m : mc} = c; // ok -- picking I.m
let {m : mc2} = c2; // ok
let {m : md} = d; // ok
