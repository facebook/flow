// @flow

declare component MyComponent();

type T1 = MyComponen
//                  ^

type T2 = renders MyComponen
//                          ^

type TypeApp<T> = T;

type T3 = TypeApp<MyComponen>
//                         ^

component Foo(xbbb: string) {
    const xaaa = 42;

    const w = x
//             ^
  }
