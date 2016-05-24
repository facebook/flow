declare class _C {
  foo(): number;
}
declare var _module: {
  C: typeof _C;
}
declare class D extends _module.C {
  foo(): string;
}
