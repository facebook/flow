/* @flow */

interface IFoo {
  foo: string;
  static bar: number;
}

class C1 implements IFoo {} //ng
class C2 implements IFoo {
  foo: string;
  static bar: string; //ng
}

/*
type Goofy = Class<IFoo>;
class C3 implements Goofy {
  name: string;
  bar: number;
} // Unreachable foo => ng

interface Flat1 {
  static foo: string;
}

type FlatGoofy = Class<Flat1>;
class C4 implements FlatGoofy {
  name: string;
  foo: string;
} // okish
*/
