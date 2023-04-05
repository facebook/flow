export type BasicConditionalType = 1 extends infer T ? string : number;
export type InferTypeOverlap = {foo: String, bar: number} extends {foo: String, bar: infer String} ? String : boolean;
export type InferTypeOverlapGenerics<T> = {foo: T, bar: number} extends {foo: T, bar: infer T} ? T : boolean;
export type InvalidInfer = infer X;
export type Exclude<T, U> = T extends U ? empty : T;
