// This is a regression test for incorrect fast-path for objects
// Previously, we enable ObjT ~> ObjT fast path if props_ids are the same,
// which causes us to incorrectly enter the fast path when the call prop has changed.

type ThunkType<R> = { (): R };
type MyThunkType<R> = ThunkType<R>;
declare const MyThunk: MyThunkType<number>;
MyThunk as ThunkType<string>; // error: number ~> string
