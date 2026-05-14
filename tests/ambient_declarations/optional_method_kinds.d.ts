// Optional method without return type — TS allows implicit any in .d.ts files.
export declare class OptionalImplicitReturn {
  m?(x: number);
  static s?(x: number);
}

// Optional method with `this is X` type guard — only allowed under MethodKind.
export declare class OptionalTypeGuard {
  isFoo?(x: mixed): this is OptionalTypeGuard;
}
