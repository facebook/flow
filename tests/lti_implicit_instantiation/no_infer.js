import * as NoInferExported from './no_infer_exported';

declare function f1<T>(v1: T, v2: T): void;
declare function f2<T>(v1: T, v2: NoInfer<T>): void;
declare function f3<T>(v1: T, v2: {f: T}): void;
declare function f4<T>(v1: T, v2: NoInfer<{f: T}>): void;

f1('1', 2); // ok
f2('1', 2); // error: number ~> string
f3('1', {f: 2}); // ok
f4('1', {f: 2}); // error: number ~> string
NoInferExported.f1('1', 2); // ok
NoInferExported.f2('1', 2); // error: number ~> string
NoInferExported.f3('1', {f: 2}); // ok
NoInferExported.f4('1', {f: 2}); // error: number ~> string

type ConditionalNoInferToplevel<T> = NoInfer<T extends infer V ? V : T>;
'3' as ConditionalNoInferToplevel<string>; // ok
3 as ConditionalNoInferToplevel<string>; // error: number ~> string

type ConditionalNoInferNested<T> = NoInfer<T extends infer V1 ? (V1 extends infer V2 ? V2 : V1) : T>;
'3' as ConditionalNoInferNested<string>; // ok
3 as ConditionalNoInferNested<string>; // error: number ~> string

declare function f5<T>(
  v1: T,
  v2: NoInfer<1 extends infer V ? V : string>
): void;
f5(1, 1); // ok
f5(1, '1'); // error: string ~> number

declare function f6<T>(
  v1: T,
  v2: NoInfer<$Call<<V>(V) => V, 1>> // error: underconstrained
): void;
f6(1, 1); // ok

declare function f7<T>(
  v1: T,
  v2: NoInfer<{[K in keyof T]: T[K]}>
): void;
f7({a: ''}, {a: ''}); // ok
f7({a: ''}, {a: 3}); // error: number ~> string

declare function f8<T>(
  v1: T,
  v2: NoInfer<$ObjMap<<V>(V) => V, {a: string}>> // error: underconstrained
): void;
f8(1, {a: ''}); // ok
