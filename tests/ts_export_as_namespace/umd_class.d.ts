declare class Thing {
  foo(): number;
}
declare namespace Thing {
  interface SubThing {
    z: boolean;
  }
}
export = Thing;
export as namespace UmdClass;
