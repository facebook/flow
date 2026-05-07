declare class _C {
  foo(): number;
}
declare const _module: {
  C: Class<_C>;
  ...
}
declare class D extends _module.C {
  foo(): string;
}
