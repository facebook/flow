declare class T {}
declare var x: T;

declare class U {}
declare var y: U;

type O1 = {...void};
declare var o1: O1;
o1 as {}; // ok
o1 as {||}; // error
o1 as {p:T}; // error
({} as O1); // ok
({p:x} as O1); // ok

type O2 = {|...void|};
declare var o2: O2;
o2 as {}; // ok
o2 as {||}; // ok
o2 as {p:T}; // error
({} as O2); // ok
({p:x} as O2); // error

type O3 = {...null};
declare var o3: O3;
o3 as {}; // ok
o3 as {||}; // error
o3 as {p:T}; // error
({} as O3); // ok
({p:x} as O3); // ok

type O4 = {|...null|};
declare var o4: O4;
o4 as {}; // ok
o4 as {||}; // ok
o4 as {p:T}; // error
({} as O4); // ok
({p:x} as O4); // error

type O5 = {...void, ...{p:T}};
declare var o5: O5;
o5 as {p?:T}; // ok
o5 as {p:T}; // error: o5.p is optional
({} as O5); // error
({p:x} as O5); // ok
({p:y} as O5); // error: y ~> T
({p:x,q:y} as O5); // ok

type O6 = {...{p:T}, ...void};
declare var o6: O6;
o6 as {p?:T}; // error, void doesn't overwrite p
o6 as {p:T}; // ok
({} as O6); // error
({p:x} as O6); // ok
({p:y} as O6); // error: y ~> T
({p:x,q:y} as O6); // ok
