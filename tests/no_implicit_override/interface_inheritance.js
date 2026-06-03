// Interfaces have no `override` modifier syntax (rejected at type-annotation
// time). The implicit-override check must skip structural sigs entirely —
// otherwise users would see an [invalid-override] error with no way to silence
// it. Regression test for the interface + NIO false positive.

interface IBase {
  greet(): string;
}

interface ISub extends IBase {
  greet(): string; // OK — no [invalid-override] under NIO
}

// Inline interface bodies have the same property.
type T = interface extends IBase {
  greet(): string; // OK
};

null as any as ISub;
null as any as T;
