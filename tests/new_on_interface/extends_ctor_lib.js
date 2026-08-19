// The exported side of the statics case: consumers see this class through the
// signature, not the checking, path — which is the path `node.js.flow`'s
// `declare class Buffer extends Uint8Array` reaches www through.

export interface Bar {
  b: string;
}

export interface StaticsCtor {
  new (): Bar;
  of(x: string): Bar;
  readonly tag: string;
}

declare const StaticsC: StaticsCtor;

export declare class FromStatics extends StaticsC {
  d: number;
}

declare class BaseCls {
  static of(x: string): Bar;
  m(): string;
}
export declare class FromCls extends BaseCls {}
