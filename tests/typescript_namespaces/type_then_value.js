// Reverse declaration order should also work.
interface A { x: number; }
const A = 1;

const v: number = A;
const w: A = { x: 1 } as A;
