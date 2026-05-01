const a = { x: 0 };
const b = { z: 0, ...a };
b.x as string;
b.z as string;

type A = {| x: number |}
export type B = {| z: number, ...A |}
module.exports = b;
