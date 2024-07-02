declare namespace S {
//                ^
  declare const a: number;
  declare const s: typeof S;
}

   S;
// ^
 S.a;
// ^
 S.s;
// ^
S.s.s;
//  ^
