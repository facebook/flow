function bar() {}

let o = { m() {}, n : function() {} }

let baz = function () {};


// all ok
bar as () => void;
baz as () => void;
o.m as () => void;
o.n as () => void;

// all ok
bar as (this : unknown) => void;
baz as (this : unknown) => void;
o.m as (this : unknown) => void;
o.n as (this : unknown) => void;

// all ok
bar as (this : empty) => void;
baz as (this : empty) => void;
o.m as (this : empty) => void;
o.n as (this : empty) => void;


function foo(this : number) {}

function bar2() {}

let o2 = { m() {}, n : function() {} }

let baz2 = function () {};

foo as typeof bar2; // mixed incompatible with number
foo as typeof baz2; // mixed incompatible with number
foo as typeof o2.m; // mixed incompatible with number
foo as typeof o2.n; // mixed incompatible with number


function this_default(
    this: {y: number, ...},
    x : number = this.y // do not infer mixed here, this counts as the function body
) {}

interface I {
    m() : void;
}

declare var i : I;

foo as typeof i.m; // method-unbinding, this type becomes any, so no more this typing errors

//$FlowExpectedError[method-unbinding]
i.m as () => void; // ok

//$FlowExpectedError[method-unbinding]
i.m as (this : unknown) => void; // ok

//$FlowExpectedError[method-unbinding]
i.m as (this : empty) => void; // ok
