/* @flow */

[1, 2] as Array<number> as Iterable<number>;
[1, 2, 'hi'] as Iterable<number | string>;
[1, 2, 3] as Iterable<any>;

['hi'] as Iterable<number>; // Error string ~> number
['hi', 1] as Iterable<string>; // Error number ~> string
