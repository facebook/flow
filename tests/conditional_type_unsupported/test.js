type BasicConditionalType = 1 extends number ? string : number; // error: unsupported
const num: BasicConditionalType = "";
