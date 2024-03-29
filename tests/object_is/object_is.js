Object.is(1, 1);
Object.is(1, 2);
Object.is(1, {});
Object.is(1, NaN);
Object.is(0, 0);
Object.is(0, -0);
Object.is(NaN, NaN);
Object.is({}, {});

var emptyObject = {};
var emptyArray: string[] = [];
Object.is(emptyObject, emptyObject);
Object.is(emptyArray, emptyArray);
Object.is(emptyObject, emptyArray);

var squared = (x: number) => x * x;
Object.is(squared, squared);

var a: boolean = Object.is('a', 'a');
var b: string = Object.is('a', 'a');
var c: boolean = Object.is('a');
var d: boolean = Object.is('a', 'b', 'c'); // Error - 'c' is unused
