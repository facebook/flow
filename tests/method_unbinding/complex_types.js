// @flow

interface I {
    m() : void
}

interface J {
    m() : void
}

declare var a : I & J;
declare var b : I | J;

a.m; // err
b.m; // err
let {m : ma} = a; // err
let {m : mb} = b; // err

interface K {
    m : () => void
}

declare var c : I & K;
declare var c2 : K & I;
declare var d : I | K;

c.m; // ok
c2.m; // ok
d.m; // err

let {m : mc} = c; // ok
let {m : mc2} = c2; // ok
let {m : md} = d; // err
