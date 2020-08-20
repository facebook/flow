//@flow

const React = require('React');
const {
  FunctionComponent,
  ClassComponent,
  Foo,
  DefaultedStringEnum,
  InitializedStringEnum,
  NumberEnum,
  BooleanEnum,
  SymbolEnum,
} = require('./objects');
import type {Props} from './objects';

let component1 =
  <FunctionComponent
    foo={1}
//   ^
    bar={2}
//   ^
    baz={3}
//   ^
    qux={4}
//   ^
  ></FunctionComponent>;

let component2 =
  <ClassComponent
    foo={1}
//   ^
    bar={2}
//   ^
    baz={3}
//   ^
    qux={4}
//   ^
  ></ClassComponent>;

(function (x : Props) {
  x.foo;
//   ^
  x.bar;
//   ^
  x.baz;
//   ^
  x.qux
//   ^
});

Foo.staticProp
//    ^
Foo.staticMethod();
//    ^
let fooInst = new Foo();
fooInst.instanceProp;
//         ^
fooInst.instanceMethod();
//         ^
fooInst.varianceProp;
//         ^

DefaultedStringEnum;
//     ^
DefaultedStringEnum.Member1;
//                    ^
DefaultedStringEnum.Member2;
//                    ^
InitializedStringEnum;
//     ^
InitializedStringEnum.Member1;
//                      ^
InitializedStringEnum.Member2;
//                      ^
NumberEnum;
//   ^
NumberEnum.Member1;
//           ^
NumberEnum.Member2;
//           ^
BooleanEnum;
//    ^
BooleanEnum.Member1;
//            ^
BooleanEnum.Member2;
//            ^
SymbolEnum;
//   ^
SymbolEnum.Member1;
//            ^
SymbolEnum.Member2;
//            ^
