/**
 * test initialization tracking for lets
 * @flow
 */

// deferred init on annotated lets is ok
function linear_deferred_init() {
  let x:number;
  x = 0;
  let y:number = x;
}

// use of let before init gives undefined
function linear_pre_init() {
  let x:number;
  let y:?number = x;  // ok
  let z:number = x;   // error
  x = 0;
  let w:number = x;   // ok
}

// self-references in let bindings are ok, mod type errors
function self_init() {
  let x:?number = x;  // ok
  let y:number = y; // error, uninitialized ~/> number
}

// use of let after partial init gives undefined
function if_partial_post_init(b) {
  let x:number;
  if (b) {
    x = 0;
  }
  var y:number = x; // error, possibly uninitialized
}

// use of let after guaranteed init is ok
function if_post_init(b) {
  let x:number;
  if (b) {
    x = 0;
  } else {
    x = 1;
  }
  var y:number = x;
}

// use in a switch after a skipped decl is an error
function switch_scoped_init_2(i) {
  switch (i) {
    case 0:
      let x:number;
    case 1:
      let y:number = x; // error, skipped declaration
  }
}

// while leaves it possibly uninitialized
function while_post_init(b) {
  let x:number;
  while (b) {
    x = 0;
  }
  var y:number = x; // error
}

// do-while is ok, because loop is guaranteed to run at least once
function do_while_post_init(b) {
  let x:number;
  do {
    x = 0;
  } while (b);
  var y:number = x; // ok
}

// for-in leaves it possibly uninitialized
function for_in_post_init() {
  var x:number;
  for (var p in {}) {
    x = 0;
  }
  var y:number = x; // error
}

// for-of leaves it possibly uninitialized
function for_of_post_init() {
  var x:number;
  for (var x of []) {
    x = 0;
  }
  var y:number = x; // error
}
