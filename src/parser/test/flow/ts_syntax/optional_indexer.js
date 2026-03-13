type T = {[string]?: number};
type S = {readonly [string]?: number};

declare class C {
  static readonly [string]?: number;
}

interface I {
  [string]?: number;
}
