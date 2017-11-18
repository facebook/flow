class A {
  abstract async nonstatic(): Promise<number>;
  abstract static async static(): Promise<string>;
}
let a = new A; //ng

class B extends A {
  nonstatic(): Promise<number> { return new Promise(resolve => resolve(5)); }
  static static() { return new Promise(resolve => resolve("a string")); }
}
let b = new B;

class C extends A {
  async nonstatic() {
    return "another string"; //ng, non-number
  }
  static async static() {
    return 13; //ng, non-string
  }
}
let c = new C;

class D extends A {
  async nonstatic() { return 7; }
  static async static() { return "yet another string"; }
}
let d = new D;
