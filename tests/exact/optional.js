function foo1(x: {}): {p?: number, ...} { return x; } // error, p must be read-only
function foo2(x: {}): {readonly p?: number, ...} { return x; } // OK
function foo3(x: {__proto__: { p: string, ...}}): {readonly p?: number, ...} { return x; } // error, type incompatibility
function foo4(x: {__proto__: { p: number, ...}}): {readonly p?: number, ...} { return x; } // OK
function foo5(x: {__proto__: { p?: number, ...}}): {readonly p?: number, ...} { return x; } // OK
