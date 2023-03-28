type BasicConditionalType = 1 extends infer T ? string : number;  // evals to string
const str: BasicConditionalType = "";  // ok
const num: BasicConditionalType = 0;  // error: number ~> string
(str: string); // ok
(str: number); // error: string ~> number

((0: 1 extends infer T ? string : number): number); // error: number ~> string, string ~> number
