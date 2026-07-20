const s = 'foo' as const;
const s2 = 'bar' as const;
declare const v: number;

// A computed field with a non-literal value and no annotation reports the usual
// missing-annotation error, exactly as an identifier-keyed field would.
class C {
  [s] = v; // ERROR: missing an annotation on property `foo`
}

// An uninitialized computed field with no annotation on an exported class also
// fails signature verification, exercising the signature builder's annotation
// requirement for computed members.
export class D {
  [s2]; // ERROR: missing annotation (fails signature verification)
}
