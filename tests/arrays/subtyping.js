{
  declare class Animal {}
  declare class Dog extends Animal {}
  declare const arr: Array<Dog>;
  (arr: Array<Animal>); // error
}

{
  declare const arr: Array<{foo: string, ...}>;
  (arr: Array<{...}>); // error
}

{
  declare const arr: Array<{foo: string}>;
  (arr: Array<{}>); // error
}
