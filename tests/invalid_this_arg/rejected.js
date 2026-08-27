type O = {
  m: (this: {x: number, ...}, y: number) => void,
};

declare const o: O;
declare const other: O;

o.m.call(other, 1); // error: receiver mismatch
o.m.apply(other, [1]); // error: receiver mismatch
o.m.bind(other); // error: receiver mismatch

o.m.call(); // error: no first argument
o.m.bind(); // error: no first argument

declare const args: Array<unknown>;
o.m.call(...args); // error: spread first argument

o.m.call(makeReceiver(), 1); // error: not a simple expression
declare function makeReceiver(): {x: number};

declare function f(this: {x: number, ...}, y: number): void;

f.call(o, 1); // error: `f` is not a member access
f.apply(o, [1]); // error: `f` is not a member access
f.bind(o); // error: `f` is not a member access
f.call(f, 1); // error: `f` is not a member access

const holder: {fn: (this: {x: number, ...}, y: number) => void} = {fn: f};
holder.fn.call(o, 1); // error: receiver mismatch, must be `holder`
