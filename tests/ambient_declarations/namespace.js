// Test ambient declarations in declare namespace blocks
declare namespace MyNamespace {
  // Implicit ambient variables (without 'declare' keyword)
  const value: number;
  let mutableValue: string;

  // Functions and classes still need 'declare' keyword
  declare function helper(x: string): number;
  declare class Helper {
    method(): void;
  }

  // Type declarations still work
  type MyType = string;
  interface MyInterface {
    prop: number;
  }
}

export {MyNamespace}
