//@flow

const React = require('React');
const {FunctionComponent, ClassComponent, Foo} = require('./objects');
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
