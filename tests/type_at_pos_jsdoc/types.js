//@flow

/** this is myFoo */
type myFoo = number;
/** this is myBar */
type myBar<T> = ?T;
/** this is myClass */
class myClass {};
/** this is myInterface */
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
// TODO: fix loc attached to enum "import type"
(x : exportValue) => {};
//    ^
