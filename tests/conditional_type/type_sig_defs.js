export type BasicConditionalType = 1 extends infer T ? string : number;
export type InferTypeOverlap = {foo: String, bar: number} extends {foo: String, bar: infer String} ? String : boolean;
export type InferTypeOverlapGenerics<T> = {foo: T, bar: number} extends {foo: T, bar: infer T} ? T : boolean;
export type InvalidInfer = infer X;
export type Exclude<T, U> = T extends U ? empty : T;
export type InferInTemplateLiteral<T> = T extends `${infer U}` ? U : T; // ERROR: Template literals are unsupported for now - testing crash
export type InferInConstructorType<T> = T extends new (x: infer U) => any ? U : T; // ERROR: Constructor types are unsupported - testing crash
export type InferInReadOnly<T> = T extends readonly {x: infer U} ? U : T; // ERROR: readonly on non-tuple/non-array is unsupported - testing crash
export type InferInTemplateLiteralAndTuple<T> = T extends [`${infer U}`, ...infer V] ? [U, V] : T; // ERROR: Template literals are unsupported for now - testing InternalError
