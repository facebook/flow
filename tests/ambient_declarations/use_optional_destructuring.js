import {obj_optional, arr_optional} from './optional_destructuring';

obj_optional({foo: 'hello'}); // OK
arr_optional(['hello']); // OK
obj_optional(); // OK - parameter is optional
arr_optional(); // OK - parameter is optional

obj_optional({foo: 123}); // ERROR: number ~> string
arr_optional([123]); // ERROR: number ~> string
