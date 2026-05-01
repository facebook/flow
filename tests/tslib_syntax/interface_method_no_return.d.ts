// Bare method in exported interface — OK in `.d.ts`, defaults to implicit `any` return
export interface I1 {
  m();
  n(x: number);
}

// Generic interface with bare method
export interface I2<T> {
  produce(x: T);
}

// Interface with optional method
export interface I3 {
  maybeRun?(x: number);
}

// Regression — explicit return type still works
export interface I4 {
  m(): number;
}
