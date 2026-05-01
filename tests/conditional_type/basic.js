type BasicConditionalType = 1 extends infer T ? string : number;  // evals to string
const str: BasicConditionalType = "";  // ok
const num: BasicConditionalType = 0;  // error: number ~> string
str as string; // ok
str as number; // error: string ~> number

0 as 1 extends infer T ? string : number as number; // error: number ~> string, string ~> number
