/* @flow */

[] as Array<string> as Iterable<?string>; // ok, Iterable<+T>

([] as Array<string>).values() as Iterable<?string>; // ok, Iterator<+T>
