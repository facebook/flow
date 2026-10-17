declare namespace Outer {
  declare namespace Inner {
    type T = number;
  }
}
type N = Outer.Inner.T;
1 as N;
true as N; // error
