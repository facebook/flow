type BasicConditionalType = 1 extends number ? string : number;  // evals to string
const num: BasicConditionalType = "";  // ok
