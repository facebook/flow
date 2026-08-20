/* @flow */

function undef_var(x: ?number) {
  if (x !== null && x !== undefined) {
    var y = x * 1000;
  }
}

function undef_var_rev(x: ?number) {
  if (x === null || x === undefined) {
  } else {
    var y = x * 1000;
  }
}

function undef_prop(x: { x: ?number, ... }) {
  if (x.x !== null && x.x !== undefined) {
    var y = x.x * 1000;
  }
}

function undef_prop_rev(x: { x: ?number, ... }) {
  if (x.x === null || x.x === undefined) {
  } else {
    var y = x.x * 1000;
  }
}

function undef_var_fail(x: ?number) {
  if (x !== undefined) {
    var y = x * 1000;
  }
}

function undef_var_fail_rev(x: ?number) {
  if (x === undefined) {
  } else {
    var y = x * 1000;
  }
}

function undef_prop_fail(x: { x: ?number, ... }) {
  if (x.x !== undefined) {
    var y = x.x * 1000;
  }
}

function undef_prop_fail_rev(x: { x: ?number, ... }) {
  if (x.x === undefined) {
  } else {
    var y = x.x * 1000;
  }
}

function undef_unreachable(x: number) {
  if (x === undefined) {
    var y = x * 1000; // unreachable
  }
  if (x == undefined) {
    var z = x * 1000; // unreachable
  }
}

function undef_var_nonstrict(x: ?number, y: ?number) {
  if (x != undefined) {
    var a = x * 1000;
  }
  if (y == undefined){
    var b = y * 1000; // error
  }
}

function undef_bogus_comparison() {
  if (100 * undefined) {
    return;
  }
  if (undefined * 100) {
    return;
  }
}

declare function pred(x: void): boolean;

// Passing `undefined` to a call refines the `undefined` binding itself.
function undef_latent_refi_on_undefined(x: ?number) {
  if (pred(undefined) && x !== null && x !== undefined) {
    var y = x * 1000;
  }
}

function undef_latent_refi_on_undefined_stmt(x: ?number) {
  pred(undefined);
  if (x !== null && x !== undefined) {
    var y = x * 1000;
  }
}

function undef_latent_refi_on_undefined_nested(x: ?number) {
  if (pred(undefined)) {
    if (x !== null && x !== undefined) {
      var y = x * 1000;
    }
  }
}

function undef_shadowed(x: ?number) {
  const undefined = 0;
  if (x !== null && x !== undefined) {
    var y = x * 1000; // error
  }
}
