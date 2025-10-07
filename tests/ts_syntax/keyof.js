type O = {a: 1, b: 2};

type T = keyof O; // OK

const keyof = 1; // OK

{
    const A = {a: 3, b: 4};
    const B = {c: 3, d: 4};
    type kk = keyof typeof A | keyof typeof B;
    const x: kk = "a"; // OK
    const y: kk = "b"; // OK
    const z: kk = "c"; // OK
    const w: kk = "d"; // OK
}
