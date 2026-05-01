declare class T {}
declare var x: T;

declare class U {}
declare var y: U;

declare var nil: {||};

// inexact: `p` may be non-own
type O1 = {...{p:T}};
declare var o1: O1;
o1 as {p?:T}; // ok
o1 as {p:T}; // error: o1.p is optional
({} as O1); // error
({p:x} as O1); // ok
({p:y} as O1); // error: y ~> T
({p:x,q:y} as O1); // ok

// inexact: optional `p`, if own, must be `T`
type O2 = {...{p?:T}};
declare var o2: O2;
o2 as {p?:T}; // ok
o2 as {p:T}; // error: o2.p is optional
({} as O2); // ok
({p:x} as O2); // ok
({p:y} as O2); // error: y ~> T
({p:x,q:y} as O2); // ok

// can't make exact from inexact (TODO: force EvalT eagerly)
type O3 = {|...{p:T}|}; ({p:x} as O3); // error: spread result is not exact

// exact
type O4 = {...{|p:T|}};
declare var o4: O4;
o4 as {p:T}; // ok
o4 as {|p:T|}; // error: not exact
({} as {}) as O4; // error: property `p` not found
({p:x} as O4); // ok
({p:y} as O4); // error: y ~> T
({p:x,q:y} as O4); // ok

// can make exact from exact
type O5 = {|...{|p:T|}|};
declare var o5: O5;
o5 as {p:T}; // ok
o5 as {|p:T|}; // ok
nil as O5; // error: property `p` not found
({p:x} as O5); // ok
({p:y} as O5); // error: y ~> T
({p:x,q:y} as O5); // error: additional property `q` found

// inexact p + exact p
type O6 = {...{p:T},...{|p:U|}};
declare var o6: O6;
o6 as {p:U}; // ok
({} as {}) as O6; // error: property `p` not found
({p:x} as O6); // error: x ~> U
({p:y} as O6); // ok
({p:y,q:x} as O6); // ok

// inexact p + exact p ~> exact (TODO: force EvalT eagerly)
type O7 = {|...{p:T},...{|p:U|}|}; ({p:y} as O7);// error: spread result is not exact

// exact p + inexact p
type O8 = {...{|p:T|},...{p:U}};
declare var o8: O8;
o8 as {p:U}; // ok
o8.p as T; // error: U ~> T


// inexact p + exact q
type O9 = {...{p:T},...{|q:U|}};
declare var o9: O9;
o9 as {p?:T,q:U};
o9.p as T; // error: o9.p is optional
o9.q as U; // ok

// exact p + inexact q
type O10 = {...{|p:T|},...{q:U}}; // Error, p may exist in second object
declare var o10: O10;
o10 as {p:any, q: any};

// inexact p + inexact q
type O11 = {...{p:T},...{q:U}}; // Error, p may exist in second object
declare var o11: O11;
o11 as {p:any, q: any}; // Error

// exact + exact
type O12 = {...{|p:T|},...{|q:U|}};
declare var o12: O12;
o12 as {p:T,q:U}; // ok

// inline properties are exact
type O13 = {...{p:T},p:U};
declare var o13: O13;
o13 as {p:U};

// exact types spread in an inexact type is inexact when spread again
type O14 = {...{...{|p:T|}}};
declare var o14: O14;
o14 as {p:T}; // error: `p` is optional
o14 as {p?:T}; // ok
o14 as {}; // ok
({p:x} as O14); // ok
({p:y} as O14); // error: U ~> T
({} as O14); // error
