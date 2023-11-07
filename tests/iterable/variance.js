/* @flow */

[] as Array<string> as Iterable<?string>; // ok, Iterable<+T>

([]: Array<string>).values() as Iterable<?string>; // ok, Iterator<+T>
