type A = {| x: number |} | {| y: number |}

const a: A = { x: 0 };
const b = { z: 0, ...a };
b.x as string; // Error, might not be on b or will be number
b.y as string; // Error, might not be on b or will be number
b.z as string; // Error number ~> string

export type B = {| z: number, ...A |}
module.exports = b;
