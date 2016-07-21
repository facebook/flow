declare class Class0 {
}
declare export class Base<A, B, C> {
  baseInst: Base<number, string, mixed>;
  childInst: Child<string, number>;

  baseMethod(a: number, b: string): number;
}

declare export class Child<A, B> extends Base<A, B, mixed> {
  notExported: Class0;
}


