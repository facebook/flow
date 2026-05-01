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

"" as BasicConditionalType; // ok
1 as BasicConditionalType; // error: number ~> string
1 as InferTypeOverlap; // ok
"" as InferTypeOverlap; // error: string ~> number
1 as InferTypeOverlapGenerics<string>; // ok
"" as InferTypeOverlapGenerics<string>; // error: string ~> number
1 as any as empty as InvalidInfer; // ok: invalid infer becomes any

declare class A {};
declare class B {};
declare class C {};
declare class D {};
type A_or_C = Exclude<A|B|C, B>;
new A() as A_or_C; // ok
new B() as A_or_C; // error: B ~> A|C
new C() as A_or_C; // ok
new D() as A_or_C; // error: D ~> A|C

"foo" as InferInTemplateLiteral<"bar">; // OK: no crash
"foo" as InferInConstructorType<string>; // OK: no crash
"foo" as InferInReadOnly<{x: string}>; // OK: no crash
["foo", ["baz"]] as InferInTemplateLiteralAndTuple<["bar", "baz"]>; // OK: no crash (previously "out of order" in compact_table)
