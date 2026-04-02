import type {
  BasicConditionalType,
  InferTypeOverlap,
  InferTypeOverlapGenerics,
  InvalidInfer,
  Exclude,
  InferInTemplateLiteral,
  InferInConstructorType,
  InferInReadOnly,
  InferInTemplateLiteralAndTuple,
} from './type_sig_defs';

("": BasicConditionalType); // ok
(1: BasicConditionalType); // error: number ~> string
(1: InferTypeOverlap); // ok
("": InferTypeOverlap); // error: string ~> number
(1: InferTypeOverlapGenerics<string>); // ok
("": InferTypeOverlapGenerics<string>); // error: string ~> number
(((1: any): empty): InvalidInfer); // ok: invalid infer becomes any

declare class A {};
declare class B {};
declare class C {};
declare class D {};
type A_or_C = Exclude<A|B|C, B>;
(new A(): A_or_C); // ok
(new B(): A_or_C); // error: B ~> A|C
(new C(): A_or_C); // ok
(new D(): A_or_C); // error: D ~> A|C

"foo" as InferInTemplateLiteral<"bar">; // OK: no crash
"foo" as InferInConstructorType<string>; // OK: no crash
"foo" as InferInReadOnly<{x: string}>; // OK: no crash
["foo", ["baz"]] as InferInTemplateLiteralAndTuple<["bar", "baz"]>; // OK: no crash (previously "out of order" in compact_table)
