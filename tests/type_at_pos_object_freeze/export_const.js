// @flow

export const obj1 = Object.freeze({f:"a"}); // {+f: "a"}
//           ^
export const obj2 = Object.freeze({f:{g:"a"}}); // {+f:{g:string}}
//           ^
export const obj3 = Object.freeze({...{f:"a"}}); // {+f: "a"}
//           ^
export const obj4 = Object.freeze({...{...{f:"a"}}}); // {+f: "a"}
//           ^
export const obj5 = Object.freeze({...{a: "a",...{b:"b", c: {d:"d"}}, e: "e"}}); // {+a: "a", +b: "b", +c: {d: string}, +e: "e"}
//           ^
export const obj6 = { prop: Object.freeze({f:"a"}) }; // {prop: {+f: "a"}}
//           ^
