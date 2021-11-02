/***
 * Test tracking of variable types across closure calls.
 * @flow
 * @noformat
 */

function takes_string(_: string) {}

// global write from function
//

var global_x = 'hello';

function global_f() {}
function global_g() {
  global_x = 42; // blame
}

global_f();
takes_string(global_x); //no error cause havoc to 'hello'

global_g();
takes_string(global_x);

global_x = 42; // blame

// local write from function
//

function local_func() {
  var local_x = 'hello';

  function local_f() {}
  function local_g() {
    local_x = 42; // blame
  }

  local_f();
  takes_string(local_x); // no error cause havoc to hello

  local_g();
  takes_string(local_x);

  local_x = 42; // error
}

// global write from method
//

var global_y = 'hello';

var global_o = {
  f: function() {},
  g: function() {
    global_y = 42; // blame
  },
};

global_o.f();
takes_string(global_y); // no error cause havoc to hello

global_o.g();
takes_string(global_y);

global_y = 42; // error

// local write from method
//

function local_meth() {
  var local_y = 'hello';

  var local_o = {
    f: function() {},
    g: function() {
      local_y = 42;
    }, // blame
  };

  local_o.f();
  takes_string(local_y); // no error cause havoc to hello

  local_o.g();
  takes_string(local_y);

  local_y = 42; // error
}

function havoc_before_decl() {
  function havoc() {
    x = 10; // error
  }
  var x = 'hello world';
  havoc();
  (x: string); // fine
}

function havoc_before_decl_annot() {
  function havoc() {
    x = 10;
  }
  var x: number | string = 'hello world';
  if (typeof x === 'string') {
    havoc();
    (x: string); // error
  }
}

function no_havoc_before_decl_annot1() {
  function no_havoc() {}
  var x = 'hello world';
  no_havoc();
  (x: string);
}

function no_havoc_before_decl_annot2() {
  function no_havoc() {}
  var x: number | string = 'hello world';
  if (typeof x === 'string') {
    no_havoc();
    (x: string);
  }
}

function havoc_uninitialized() {
  var x: number;

  function havoc() {
    x = 42;
  }
  havoc();
  (x: void); // should error
}

function havoc_undeclared() {
  x; // expected error
  function havoc() {
    x = 42;
  }
  havoc();
  (x: void); // should error
}

function havoc_not_yet_declared() {
  function havoc() {
    x = 42;
  }
  havoc();
  let x: ?number = null;
  x;
}
