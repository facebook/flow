// @flow

class A {
    m() {}
    n = () => {}
}

class B extends A {
    x() {}
}

interface I {
    m() : void;
    n : () => void;
}

type J = interface {
    m() : void;
    n : () => void;
}

type O = {
    m() : void;
    n : () => void;
}

let a = new A();
let b = new B();
declare var i : I;
declare var j : J;
declare var o  : O;

/* tests */

a.m; // err
a.n; // ok

b.m; // err
b.n; // ok
b.x; // err

i.m; // err
i.n; // ok

j.m; // err
j.n; // ok

o.m; // ok
o.n; // ok

let {m : mb} = b; // err
let {x : xb} = b; // err
let {n : nb} = b; // ok

let {m : ma} = a; // err
let {n : na} = a; // ok

let {m : mi} = i; // err
let {n : ni} = i; // ok

let {m : mj} = j; // err
let {n : nj} = j; // ok

let {m : mo} = o; // ok
let {n : no} = o; // ok

a.m(); // ok
a.n(); // ok

b.m(); // ok
b.x(); // ok
b.n(); // ok

i.m(); // ok
i.n(); // ok

j.m(); // ok
j.n(); // ok

o.m(); // ok
o.n(); // ok

let x1 = {...a}; // ok (methods not own)
let x2 = {...b}; // ok (methods not own)
let x3 = {...i}; // cannot spread interface
let x4 = {...j}; // cannot spread interface
let x5 = {...o}; // ok

if (a.m) {} // err
if (a.n) {} // ok

if (b.m) {} // err
if (b.n) {} // ok
if (b.x) {} // err

if (j.m) {} // err
if (j.n) {} // ok

if (i.m) {} // err
if (i.n) {} // ok


type T = {
  f: string => void,
};

import X from './lib.js';

const ob : T = {
    f: X.method,
};
