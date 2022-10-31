// @flow

declare var arr: Array<string>;

(arr.at(0): string | void); // OK
(arr.at(1): string | void); // OK
(arr.at(-1): string | void); // OK
arr.at(); // ERROR
arr.at("1"); // ERROR

declare var roArr: $ReadOnlyArray<string>;

(roArr.at(0): string | void); // OK
(roArr.at(1): string | void); // OK
(roArr.at(-1): string | void); // OK
roArr.at(); // ERROR
roArr.at("1"); // ERROR
