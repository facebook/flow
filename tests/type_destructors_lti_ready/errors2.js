/**
 * @format
 * @flow
 */

null as {}['p']; // Error should point here.

type A = {}['p']; // Error should point here.
null as A;

type B<O> = O['p'];
type C = B<{}>; // Error should point here.
null as C;

declare function f1<O>(o: O): {}['p']; // Error should point here.
declare function f2<O>(o: O): A;
declare function f3<O>(o: O): B<{}>; // Error should point here.
declare function f4<O>(o: O): O['p'];
declare function f5<O>(o: O): B<O>;

declare var o: {};
f1(o) as empty;
f2(o) as empty;
f3(o) as empty;
f4(o) as empty; // Error should point here.
f5(o) as empty; // Error should point here.
