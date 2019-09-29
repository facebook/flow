// @flow

type A = {| x: number |} | {| y: number |}

const a: A = { x: 0 };
const b = { z: 0, ...a };
(b.x: string);
(b.y: string);
(b.z: string);

export type B = {| z: number, ...A |}
module.exports = b;
