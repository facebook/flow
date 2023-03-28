{
  type ToArray<T> = T extends any ? Array<T> : empty;

  declare const unionArr: Array<string | number>;
  declare const arrUnion: Array<number> | Array<string>;

  // TODO: distributed conditional type is not implemented yet
  (arrUnion: ToArray<string | number>); // error: TODO should be ok
  (unionArr: ToArray<string | number>); // ok: TODO should error
}
