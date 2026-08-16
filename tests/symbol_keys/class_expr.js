// Symbol-typed computed keys work in a class expression too.

declare const key1: unique symbol;
declare const key2: unique symbol;

const C = class {
  [key1]: number = 1;
  [key2](): string {
    return 's';
  }
};

const c = new C();
c[key1] as number; // OK
c[key2]() as string; // OK
c[key1] as string; // ERROR: number is incompatible with string
