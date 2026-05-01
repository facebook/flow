var { ["key"]: val1 } = { key: "val" };
val1 as void; // error: string ~> void

var key: string = "key";
var { [key]: val2 } = { key: "val" }; // error: unsafe string key access
val2 as void; // ok: due to val2 being empty

var { ["key"]: val3, ...spread } = { key: "val" };
spread.key as void; // error (gasp!) in general we don't know if a computed prop should be excluded from spread

const arr = [{foo: 42 as const}, {foo: 17 as const}];
const index = 1;
const {[index]: {foo}} = arr; // okay
foo as 42; // error 17 ~> 42
foo as 17; // okay 17 ~> 17
foo as string; // error 17 ~> string
