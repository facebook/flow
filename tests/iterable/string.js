(new String("hi"): Iterable<string>);
(new String("hi"): Iterable<any>);
(new String("hi"): Iterable<number>); // Error - string is a Iterable<string>
