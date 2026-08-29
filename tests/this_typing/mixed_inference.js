import type {InlineI} from './interface';

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

declare const i : I;

foo as typeof i.m; // error: number receiver is incompatible with I

i.m as () => void; // error: I receiver is not satisfied by unknown

i.m as (this : unknown) => void; // error: I receiver is not satisfied by unknown

i.m as (this : empty) => void; // ok

class IImpl implements I {
    x: number = 0;

    m(): void {
        this.x;
    }
}

const iImpl: I = new IImpl();
const unboundM = iImpl.m;
unboundM(); // error: a standalone call does not provide the required I receiver

declare const inlineI: interface {m(): void};
const unboundInlineM = inlineI.m;
unboundInlineM(); // error: a standalone call does not provide the required receiver

declare const importedInlineI: InlineI;
const unboundImportedInlineM = importedInlineI.m;
unboundImportedInlineM(); // error: imported inline interfaces preserve their receiver too
