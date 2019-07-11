interface Array<T> {
  bar: string;
}

declare module 'wat' {
  declare interface Bar {
    bar: string;
  }
}
