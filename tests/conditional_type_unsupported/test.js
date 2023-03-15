type BasicConditionalType = 1 extends infer T ? string : number; // error: unsupported
const num: BasicConditionalType = "";
