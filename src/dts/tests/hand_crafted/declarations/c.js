declare module Module1 {

  declare class A {

  }
}
declare module Module2 {
  declare var Module1: $Exports<'Module1'>;
  declare var exports: typeof Module1;
}
