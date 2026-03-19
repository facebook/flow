declare namespace N {
  declare type infer = number;
}

declare const x: N.infer;
x as number; // OK
x as empty; // ERROR
