// @flow

const ok1 = (x: mixed) => typeof x === "number";
//    ^
const ok2 = (x: mixed) => typeof x === "string";
//    ^
const ok3 = (x: mixed) => ok1(x);
//    ^
const ok4 = (x: mixed) => ok1(x) || ok2(x);
//    ^
const ok5 = (x: number | "") => typeof x === "number";
//    ^
const ok6 = (x: number) => x === 42;
//    ^
const ok7 = (x: 1 | 2) => x === 1;
//    ^
const ok8 = (x: 1 | 2) => x !== 1;
//    ^
const ok9 = (x: 'a' | 'b') => x === 'a';
//    ^
const ok10 = (x: 'a'| 'b') => x !== 'a';
//    ^
const ok11 = (x: boolean) => x === true;
//    ^
const ok12 = (x: boolean) => x !== false;
//    ^

const no0 = (x: number) => typeof x === "number"; // non-refining
//    ^
const no1 = (x: mixed) => x; // truthy
//    ^
const no2 = (x: boolean) => x; // truthy
//    ^
const no3 = (x: mixed) => x !== 1; // non-refining
//    ^
const no4 = (x: mixed) => x && x === 1; // truthy (complex)
//    ^
const no5 = (x: null | 1 | 2) => x && x === 1; // truthy (complex)
//    ^
const no6 = (x: mixed) => x || ok1(x) || ok2(x); // truthy (complex)
//    ^
const no7 = (x: mixed) => x !== 1; // non-refining (result same as input)
//    ^
const no8 = (x: mixed) => ok1(x) || !ok2(x);
//    ^
