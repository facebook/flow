// @flow

type A = {type: 'a'}
type B = {type: 'b'}
type C = {type: 'c'}
type D = {type: 'd'}

type AB = A | B;
type CD = C | D;

type ABCD = AB | CD;
type ABCDType = ABCD["type"];
//   ^

function foo<T: ABCD>(x: T["type"]): void {}
//                    ^
