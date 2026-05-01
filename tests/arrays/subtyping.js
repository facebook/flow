{
  declare class Animal {}
  declare class Dog extends Animal {}
  declare const arr: Array<Dog>;
  arr as Array<Animal>; // error
}

{
  declare const arr: Array<{foo: string, ...}>;
  arr as Array<{...}>; // error
}

{
  declare const arr: Array<{foo: string}>;
  arr as Array<{}>; // error
}
