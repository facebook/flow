// @flow


function bar() {}

let o = { m() {}, n : function() {} }

let baz = function () {};


// all ok
(bar : () => void);
(baz : () => void);
(o.m : () => void);
(o.n : () => void);

// all ok
(bar : (this : mixed) => void);
(baz : (this : mixed) => void);
(o.m : (this : mixed) => void);
(o.n : (this : mixed) => void);

// all ok
(bar : (this : empty) => void);
(baz : (this : empty) => void);
(o.m : (this : empty) => void);
(o.n : (this : empty) => void);


function foo(this : number) {}

function bar2() {}

let o2 = { m() {}, n : function() {} }

let baz2 = function () {};

(foo : typeof bar2); // mixed incompatible with number
(foo : typeof baz2); // mixed incompatible with number
(foo : typeof o2.m); // mixed incompatible with number
(foo : typeof o2.n); // mixed incompatible with number


function this_default(
    x : number = this.y // do not infer mixed here, this counts as the function body
) {}
