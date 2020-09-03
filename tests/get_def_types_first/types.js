//@flow

type myFoo = number;
type myBar<T> = ?T;
class myClass {};
interface myInterface {};
import type {ExportFoo, ExportBar, ExportClass, ExportInterface, ExportEnum} from './type-exports';
import { typeof exportValue } from './type-exports';

(x : myFoo) => {};
//    ^
(x : myBar<string>) => {};
//    ^
(x : myClass) => {};
//    ^
(x : myInterface) => {};
//    ^
(x : ExportFoo) => {};
//    ^
(x : ExportBar<string>) => {};
//    ^
(x : ExportClass) => {};
//    ^
(x : ExportInterface) => {};
//    ^
(x : ExportEnum) => {};
//    ^
(x : exportValue) => {};
//    ^
