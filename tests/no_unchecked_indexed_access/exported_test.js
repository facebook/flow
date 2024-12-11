require('./arr_test').typeTest as string; // ok
require('./obj_test').typeTest as string; // ok
require('./arr_test').typeTest as empty; // error: sanity check
require('./obj_test').typeTest as empty; // error: sanity check
