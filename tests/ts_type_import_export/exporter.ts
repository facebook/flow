export type MyType = number;
export interface MyInterface { x: string }
declare namespace MyNamespace {
  declare const x: number;
}
export type { MyNamespace };
