declare function m(): m.M;
declare namespace m {
  interface M {
    y: string;
  }
}
export = m;
export as namespace UmdCjs;
