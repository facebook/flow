declare function invariant(condition?: unknown, message?: string): asserts condition;

declare const a: true;
invariant(a); // constant-condition error

declare const b: false;
invariant(b); // constant-condition error

declare const c: boolean;
invariant(c);
c ? invariant(c) : invariant(c); // constant-condition error on `c`; invariant inside an expression is not a supported position

declare const d: 0;
invariant(d);

declare const e: 1;
invariant(e);

declare const f: number;
invariant(f);

declare const g: false & true;
invariant(g); // constant-condition error

declare const h: false | true;
invariant(h);
