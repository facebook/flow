declare namespace ns {
  declare interface IFace {
    method(): string;
  }
}

declare class Impl implements ns.IFace { // OK
  method(): string;
}

declare const impl: Impl;
impl.method() as string; // OK
