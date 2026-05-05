// @flow

const ok1 = (x: unknown) => typeof x === "number";
//    ^
const ok2 = (x: unknown) => typeof x === "string";
//    ^
const ok3 = (x: unknown) => ok1(x);
//    ^
const ok4 = (x: unknown) => ok1(x) || ok2(x);
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
const no1 = (x: unknown) => x; // truthy
//    ^
const no2 = (x: boolean) => x; // truthy
//    ^
const no3 = (x: unknown) => x !== 1; // non-refining
//    ^
const no4 = (x: unknown) => x && x === 1; // truthy (complex)
//    ^
const no5 = (x: null | 1 | 2) => x && x === 1; // truthy (complex)
//    ^
const no6 = (x: unknown) => x || ok1(x) || ok2(x); // truthy (complex)
//    ^
const no7 = (x: unknown) => x !== 1; // non-refining (result same as input)
//    ^
const no8 = (x: unknown) => ok1(x) || !ok2(x);
//    ^
