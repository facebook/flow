// @flow

import typeof ImportedDefaultFunction from "./exports-default-function";

import typeof ImportedDefaultInstance from "./exports-default-instance";
import typeof ImportedDefaultPolyInstance from "./exports-default-poly-instance";
import type ImportedTypeDefaultClass from "./exports-default-class";
import type ImportedTypeDefaultClassTwice from "./exports-default-class";
import type ImportedTypeDefaultPolyClass from "./exports-default-poly-class";
import typeof ImportedTypeofDefaultClass from "./exports-default-class";
import typeof ImportedTypeofDefaultClassTwice from "./exports-default-class";
import typeof ImportedTypeofDefaultPolyClass from "./exports-default-poly-class";
import typeof ImportedTypeofDefaultClassFuncall from "./exports-default-class-funcall-a";
import ImportedValueDefaultClassFuncall from "./exports-default-class-funcall-b";
import { returnsList as f9 } from './exports-list';

import type ImportedTypeOfClassC from './exports-default-class-1';
import { typeof_c } from './exports-default-class-1-generator';

declare function f1(): ImportedDefaultInstance;
declare function f2(): ImportedDefaultPolyInstance;
declare function f3(): ImportedTypeDefaultClass;
declare function f4(): ImportedTypeDefaultPolyClass<number>;
declare function f5(): ImportedTypeofDefaultClass;
declare function f6(): ImportedTypeofDefaultPolyClass;
declare function f7(): ImportedTypeofDefaultClassFuncall;
declare function f8(): ImportedValueDefaultClassFuncall;

var C = 1;

const Immutable = {
  List: {}
};
const ImmutableList = Immutable.List;

declare var immutableList: Immutable.List;

module.exports = {
  x1: f1(),
  x2: f2(),
  x3: f3(),
  x4: f4(),
  x5: f5(),
  x6: f6(),
  typeof_c: typeof_c(),  // cannot use `ImportedTypeOfClassC` -- need to import C as typeof
  x7: f7(),
  x8: f8(),
  x9: f9(),
};
