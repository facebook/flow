/**
 * @format
 * @flow
 */

null as $PropertyType<{}, 'p'>; // Error should point here.

type A = $PropertyType<{}, 'p'>; // Error should point here.
null as A;

type B<O> = $PropertyType<O, 'p'>;
type C = B<{}>; // Error should point here.
null as C;

declare function f1<O>(o: O): $PropertyType<{}, 'p'>; // Error should point here.
declare function f2<O>(o: O): A;
declare function f3<O>(o: O): B<{}>; // Error should point here.
declare function f4<O>(o: O): $PropertyType<O, 'p'>;
declare function f5<O>(o: O): B<O>;

declare var o: {};
f1(o) as empty;
f2(o) as empty;
f3(o) as empty;
f4(o) as empty; // Error should point here.
f5(o) as empty; // Error should point here.
