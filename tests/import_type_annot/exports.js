export class MyClass {
  value: number;
  constructor() {
    this.value = 42;
  }
}
export const myInstance: MyClass = new MyClass();

export type MyType = {
  name: string,
  count: number,
};


export class Nested {
  static Inner: typeof MyClass = MyClass;
}
