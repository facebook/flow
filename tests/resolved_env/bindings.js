//@flow

var x = 42;
(x: string); // num </: string

var y: number = "a"; // error
(y: string); // err

var [a, ...rest1] = [1,2,3];
(a: string); // num </: string
(rest1[0]: string); // num </: string

var {w, ...rest2} = { w: 42, p: 100 };
(w: string); // err
(rest2: { p: number });
(rest2.p: string); //err

try { }
catch (e) {
    (e: empty); // fine, e is any
    e = 10; // fine, e is any
}

for (var of_ of [1,2,3]) {
    (of_: string); //err
    (of_: number);
}

for (var in_ in { a: 42 }) {
    (in_: number); //err
    (in_: string);
}
