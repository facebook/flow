declare module P {

  declare class B {

  }
}
declare module O {

  declare class D {

  }
}
declare module N {
  declare var P: $Exports<'P'>;
  declare var O: $Exports<'O'>;
  declare var x: P.A;
  declare var z: O.D;
}
