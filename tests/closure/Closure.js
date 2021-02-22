/***
 * Test tracking of variable types across closure calls.
 * @flow
 */

function takes_string(_:string) { }

// global write from function
//

var global_x = "hello";

function global_f() { }
function global_g() { global_x = 42; } // blame

global_f();
takes_string(global_x); // error (can't distinguish between calls to global_f and global_g)

global_g();
takes_string(global_x);

global_x = 42;

// local write from function
//

function local_func() {

  var local_x = "hello";

  function local_f() { }
  function local_g() { local_x = 42; } // blame

  local_f();
  takes_string(local_x); // error (can't distinguish between calls to local_f and local_g)

  local_g();
  takes_string(local_x); // error

  local_x = 42;
}

// global write from method
//

var global_y = "hello";

var global_o = {
  f: function() { },
  g: function() { global_y = 42; } // blame
}

global_o.f();
takes_string(global_y); // error (can't distinguish between calls to global_o.f and global_o.g)

global_o.g();
takes_string(global_y); // error

global_y = 42;

// local write from method
//

function local_meth() {

  var local_y = "hello";

  var local_o = {
    f: function() { },
    g: function() { local_y = 42; } // blame
  }

  local_o.f();
  takes_string(local_y); // error (can't distinguish between calls to local_o.f and local_o.g)

  local_o.g();
  takes_string(local_y); // error

  local_y = 42;
}
