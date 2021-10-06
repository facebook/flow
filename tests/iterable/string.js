/* @flow */

("hi": Iterable<string>);
("hi": Iterable<any>);
("hi": Iterable<number>); // Error - string is a Iterable<string>
