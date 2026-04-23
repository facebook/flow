// `const A` (value) and `interface A` (type) coexist in different namespaces.
const A = 1;
interface A { x: number; }

// Value position resolves to the const.
const v: number = A;

// Type position resolves to the interface.
const w: A = ({ x: 1 }: A);
