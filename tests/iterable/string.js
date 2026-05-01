new String("hi") as Iterable<string>;
new String("hi") as Iterable<any>;
new String("hi") as Iterable<number>; // Error - string is a Iterable<string>
