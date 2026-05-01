declare class T {};

type O1 = {...{-p:T}};
declare var o1: O1;
o1 as {p?:mixed}; // ok
o1 as {p?:T}; // error: unknown ~> T
o1.p as T; // errors: undefined ~> T, unknown ~> T

type O2 = {...{-[string]:T}};
declare var o2: O2;
o2 as {[string]:mixed}; // ok
o2 as {[string]:T}; // error: unknown ~> T
o2.p as T; // errors: unknown ~> T

type O3 = {...{||}, -p: T};
declare var o3: O3;
o3 as {p:mixed}; // ok
o3 as {p:T}; // error: unknown ~> T
o3.p as T; // errors: unknown ~> T
