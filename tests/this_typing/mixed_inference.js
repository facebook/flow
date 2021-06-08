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

interface I {
    m() : void;
}

declare var i : I;

//$FlowExpectedError[method-unbinding]
(foo : typeof i.m); // mixed incompatible with number

//$FlowExpectedError[method-unbinding]
(i.m : () => void); // ok

//$FlowExpectedError[method-unbinding]
(i.m  : (this : mixed) => void); // ok

//$FlowExpectedError[method-unbinding]
(i.m  : (this : empty) => void); // ok
