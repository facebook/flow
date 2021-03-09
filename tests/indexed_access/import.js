import type {ObjFoo} from './test';

(true: ObjFoo); // OK
(1: ObjFoo); // Error
