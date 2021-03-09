// @flow

let x = { m() {} };
x.m = () => {}; // error: m is read-only

let x2 : {m() : void } = { m() {} };
x2.m = () => {}; // error: m is read-only

let y = {...x};
y.m = () => {}; // error: m is read-only

let z = { m : () => {}, ...x };
z.m = () => {}; // error: m is read-only

let z2 = {  ...x, m : () => {}, };
z2.m = () => {}; // ok, m is a function property
