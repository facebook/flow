// @flow

import typeof ImportedDefaultFunction from './exports-default-function';

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

declare const f1: ImportedDefaultInstance; // (Remote) A
//            ^
declare const f2: ImportedDefaultPolyInstance; // (Remote) P<number>
//            ^
declare const f3: ImportedTypeDefaultClass; // ImportedTypeDefaultClass
//            ^
declare const f4: ImportedTypeDefaultPolyClass<number>; // ImportedTypeDefaultPolyClass<number>
//            ^
declare const f5: ImportedTypeofDefaultClass; // ImportedTypeofDefaultClass
//            ^
declare const f6: ImportedTypeofDefaultPolyClass; // ImportedTypeofDefaultPolyClass<number>
//            ^
declare const f7: ImportedTypeofDefaultClassFuncall; // ImportedTypeofDefaultClassFuncall
//            ^
declare const f8: ImportedValueDefaultClassFuncall; // ImportedValueDefaultClassFuncall
//            ^

declare function f7_(): ImportedTypeofDefaultClassFuncall;
const r7 = f7_();
//    ^
