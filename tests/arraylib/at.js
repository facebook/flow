declare var arr: Array<string>;

arr.at(0) as string | void; // OK
arr.at(1) as string | void; // OK
arr.at(-1) as string | void; // OK
arr.at(); // ERROR
arr.at("1"); // ERROR

declare var roArr: ReadonlyArray<string>;

roArr.at(0) as string | void; // OK
roArr.at(1) as string | void; // OK
roArr.at(-1) as string | void; // OK
roArr.at(); // ERROR
roArr.at("1"); // ERROR
