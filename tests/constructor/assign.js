declare const obj: {};
obj.constructor = () => {}; // okay
obj.constructor = <T>(x: T): T => x; // okay
obj.constructor = 1; // error number ~> function
obj.constructor = null; // error null ~> function
