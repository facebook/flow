function l(x: any): implies x is number { throw "unsupported" }; // error unsupported

type A = (x: any) => implies x is number;
