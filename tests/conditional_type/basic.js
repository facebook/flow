type BasicConditionalType = 1 extends infer T ? string : number;  // evals to string
const num: BasicConditionalType = "";  // ok
