type SomeOtherType = number;

export declare type X = SomeOtherType; // OK in .d.ts: no unnecessary-declare-type-only-export

export declare interface I {
  baz: string;
}
