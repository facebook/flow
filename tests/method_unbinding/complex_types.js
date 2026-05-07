interface I {
    m() : void
}

interface J {
    m() : void
}

declare const a : I & J;
declare const b : I | J;

a.m; // err
b.m; // err
let {m : ma} = a; // err
let {m : mb} = b; // err

interface K {
    m : () => void
}

declare const c : I & K;
declare const c2 : K & I;
declare const d : I | K;

c.m; // error -- picking I.m
c2.m; // ok
d.m; // err

let {m : mc} = c; // err -- picking I.m
let {m : mc2} = c2; // ok
let {m : md} = d; // err
