interface Receiver {
  value: number;
  identity<A>(this: A): A;
  defaults<D extends number = 1>(depth?: D): D;
  widen<T>(value: T): T;
  requiresString(this: string): number;
}

declare const receiver: Receiver;
