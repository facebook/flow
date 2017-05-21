/* @noflow */

interface IFoo {
  foo: string;
  static bar: number;
}

class C1 implements IFoo {} //ng
class C2 implements IFoo {
  foo: string;
  static bar: string; //ng
}
