// @flow

declare component MyComponent();
declare const MyComponent2: component();
declare const MyComponent3: component() renders MyComponent;

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
