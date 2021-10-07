//@flow

var call_me: () => void = () => {};

function g(x: ?number) {
  var var_x = x;
  if (var_x) {
    // error: var_x might no longer be truthy when call_me is called
    call_me = () => {
      var y:number = var_x;
    };  // error
  }
  var_x = null;
}

function havoc_uninitialized() {
  var x: void | number;

  function havoc() {
    x = undefined;
  }
  havoc();
  (x: void); // should error
}

function havoc_annotated() {
  var x: number | string = 42;
  function havoc() {
    x = "hello"
  }
  if (typeof x === 'number') {
    havoc();
    (x: number); // should fail
  }
}
