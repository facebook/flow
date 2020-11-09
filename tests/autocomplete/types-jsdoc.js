//@flow

/** this is myFoo */
type myFoo = number;
/** this is myBar */
type myBar<T> = ?T;
/** this is myClass */
class myClass {};
/** this is myInterface */
interface myInterface {};
/** this is myEnum */
enum myEnum { Inl, Inr }
import type {ExportFoo, ExportBar, ExportClass, ExportInterface, ExportEnum} from './type-exports-jsdoc';
import { typeof exportValue } from './type-exports-jsdoc';

(x :  ) => {};
//   ^
