class NoProps {}

class NoReadProps {
  set a(value: number) {}
}

class OneProp {
  a: number;

  method() {}
}

class SomeProps {
  a: number;
  b: string;
  c: string;

  method() {}
}

class Parent { a: string; }
class Child extends Parent { b: number; }

'yo' as Values<NoProps>; // Error: There are no properties.
123 as Values<NoProps>; // Error: There are no properties.
(() => {}) as Values<NoProps>; // Error: There are no properties.
true as Values<NoProps>; // Error: There are no properties.

'yo' as Values<NoReadProps>; // Error: There are no readable properties.
123 as Values<NoReadProps>; // Error: There are no readable properties.
(() => {}) as Values<NoReadProps>; // Error: There are no readable properties.
true as Values<NoReadProps>; // Error: There are no readable properties.

'yo' as Values<OneProp>; // Error: There is no property with the type of
                          // string.
123 as Values<OneProp>; // OK: There is a property with the type of number.
(() => {}) as Values<OneProp>; // Error: Even though there is a method, methods
                                // are on the prototype.
true as Values<OneProp>; // Error: There is no property with the type of
                          // boolean.

'yo' as Values<SomeProps>; // OK: There is a property with the type of string.
123 as Values<SomeProps>; // Ok: There is a property with the type of number.
(() => {}) as Values<SomeProps>; // Error: Even though there is a method,
                                  // methods are on the prototype.
true as Values<SomeProps>; // Error: There is no property with the type of
                            // boolean.

'yo' as Values<Child>; // TODO: This should be ok since there is a property
                        // with the type of string on the parent.
123 as Values<Child>; // OK: There is a property with the type of number.
(() => {}) as Values<Child>; // Error: There is no property with the type of
                              // function.
true as Values<Child>; // Error: There is no property with the type of boolean.
