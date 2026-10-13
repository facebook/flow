declare namespace LibMergedClass {
  interface Foo {
    a: number;
  }
}
declare namespace LibMergedClass {
  interface Foo {
    b: string;
  }
  declare class Foo {
    c: boolean;
  }
}
