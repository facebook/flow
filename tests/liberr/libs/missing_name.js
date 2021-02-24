// @flow

declare class C {

  p: P; // okay, P is declare in def.js

  // $FlowFixMe[cannot-resolve-name]
  q: Q; // error is suppressed

  // $FlowFixMe
  r: R;  // warning missing code

  s: S; // error, S is not defined

}
