declare var LibCtor: {
  new (x: number): {x: number},
  prototype: {x: number},
  helper(): string,
};

// Checking the library sees the same interface: it cannot be made exact.
declare type LibExactCtor = $Exact<{new (): {x: number}}>; // ERROR
